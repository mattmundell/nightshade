;;; The programmer interface for writing debugging tools.

(in-package "DEBUG-INTERNALS")

;;; The compiler's debug-source structure is almost exactly what we want,
;;; so just get these symbols and export them.
;;;
(import '(c::debug-source-from c::debug-source-name c::debug-source-created
	  c::debug-source-compiled c::debug-source-start-positions
	  c::make-debug-source c::debug-source c::debug-source-p))

(export '(debug-variable-name debug-variable-package debug-variable-symbol
	  debug-variable-id debug-variable-value debug-variable-validity
	  debug-variable-valid-value debug-variable debug-variable-p

	  top-frame frame-down frame-up flush-frames-above frame-debug-function
	  frame-code-location eval-in-frame return-from-frame frame-catches
	  frame-number frame frame-p

	  do-debug-function-blocks debug-function-lambda-list
	  debug-variable-info-available do-debug-function-variables
	  debug-function-symbol-variables ambiguous-debug-variables
	  preprocess-for-eval function-debug-function debug-function-function
	  debug-function-kind debug-function-name debug-function
	  debug-function-p debug-function-start-location

	  do-debug-block-locations debug-block-successors debug-block
	  debug-block-p debug-block-elsewhere-p

	  make-breakpoint activate-breakpoint deactivate-breakpoint
	  breakpoint-active-p breakpoint-hook-function breakpoint-info
	  breakpoint-kind breakpoint-what breakpoint breakpoint-p
	  delete-breakpoint function-end-cookie-valid-p

	  code-location-debug-function code-location-debug-block
	  code-location-top-level-form-offset code-location-form-number
	  code-location-debug-source code-location-kind
	  code-location code-location-p code-location-unknown-p code-location=

	  debug-source-from debug-source-name debug-source-created
	  debug-source-compiled debug-source-root-number
	  debug-source-start-positions form-number-translations
	  source-path-context debug-source debug-source-p

	  debug-condition no-debug-info no-debug-function-returns
	  no-debug-blocks no-debug-variables lambda-list-unavailable

	  debug-error unhandled-condition invalid-control-stack-pointer
	  unknown-code-location unknown-debug-variable invalid-value
	  ambiguous-variable-name frame-function-mismatch

	  set-breakpoint-for-editor set-location-breakpoint-for-editor
	  delete-breakpoint-for-editor

	  *debugging-interpreter*))


#[ Debugger Programmer Interface

The debugger programmers interface is exported from the "DEBUG-INTERNALS"
or "DI" package.  This package provides writers of inspection tools with an
abstract interface to the details of the compiler and run-time system.

Some of the interface routines take a code-location as an argument.  As
described in the section on code-locations, some code-locations are
unknown.  When a function calls for a basic-code-location, it takes either
type, but when it specifically names the argument code-location, the
routine will signal an error if you give it an unknown code-location.

[ DI Exceptional Conditions    ]
[ Debug-variables              ]
[ Frames                       ]
[ Debug-functions              ]
[ Debug-blocks                 ]
[ Breakpoints                  ]
[ Code-locations               ]
[ Debug-sources                ]
[ Source Translation Utilities ]
]#


#[ DI Exceptional Conditions

Some of these operations fail depending on the availability of debugging
information.  In the most severe case, when someone saved a Lisp image
stripping all debugging data structures, no operations are valid.  In this
case, even backtracing and finding frames is impossible.  Some interfaces
can simply return values indicating the lack of information, or their
return values are naturally meaningful in light missing data.  Other
routines, as documented below, will signal serious-condition's when they
discover awkward situations.  This interface does not provide for programs
to detect these situations other than by calling a routine that detects
them and signals a condition.  These are serious-conditions because the
program using the interface must handle them before it can correctly
continue execution.  These debugging conditions are not errors since it is
no fault of the programmers that the conditions occur.

[ Debug-conditions ]
[ Debug-errors     ]
]#


;;;; Conditions.

#[ Debug-conditions

The debug internals interface signals conditions when it can't adhere
to its contract.  These are serious-conditions because the program
using the interface must handle them before it can correctly continue
execution.  These debugging conditions are not errors since it is no
fault of the programmers that the conditions occur.  The interface
does not provide for programs to detect these situations other than
calling a routine that detects them and signals a condition.

{condition:di:debug-condition}
{condition:di:no-debug-info}
{condition:di:no-debug-function-returns}
{condition:di:no-debug-blocks}
{condition:di:no-debug-variables}
{condition:di:lambda-list-unavailable}
{condition:di:invalid-value}
{condition:di:ambiguous-variable-name}
]#

;;; The interface to building debugging tools signals conditions that
;;; prevent it from adhering to its contract.  These are serious-conditions
;;; because the program using the interface must handle them before it can
;;; correctly continue execution.  These debugging conditions are not
;;; errors since it is no fault of the programmers that the conditions
;;; occur.  The interface does not provide for programs to detect these
;;; situations other than calling a routine that detects them and signals a
;;; condition.  For example, programmers call A which may fail to return
;;; successfully due to a lack of debug information, and there is no B that
;;; they could have called to realize A would fail.  It is not an error to
;;; have called A, but it is an error for the program to then ignore the
;;; signal generated by A since it cannot continue without A's correctly
;;; returning a value or performing some operation.
;;;
;;; Use DEBUG-SIGNAL to signal these conditions.

(define-condition debug-condition (serious-condition)
  ()
  (:documentation
   "All debug-conditions inherit from this type.  These are serious
    conditions that must be handled, but they are not programmer errors."))

(define-condition no-debug-info (debug-condition)
  ()
  (:documentation "There is absolutely no debugging information available.")
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (fresh-line stream)
	     (write-line "No debugging information available." stream))))

(define-condition no-debug-function-returns (debug-condition)
  ((debug-function :reader no-debug-function-returns-debug-function
		   :initarg :debug-function))
  (:documentation
   "The system could not return values from a frame with debug-function since
    it lacked information about returning values.")
  (:report (lambda (condition stream)
	     (let ((fun (debug-function-function
			 (no-debug-function-returns-debug-function condition))))
	       (format stream
		       "~&Cannot return values from ~:[frame~;~:*~S~] since ~
			the debug information lacks details about returning ~
			values here."
		       fun)))))

(define-condition no-debug-blocks (debug-condition)
  ((debug-function :reader no-debug-blocks-debug-function
		   :initarg :debug-function))
  (:documentation "Indicates that a function was not compiled with
		   debug-block information, and this information is
		   necessary for some requested operation.")
  (:report (lambda (condition stream)
	     (format stream "~&~S has no debug-block information."
		     (no-debug-blocks-debug-function condition)))))

(define-condition no-debug-variables (debug-condition)
  ((debug-function :reader no-debug-variables-debug-function
		   :initarg :debug-function))
  (:documentation "Indicates that a function was not compiled with
		   debug-variable information, and this information is
		   necessary for some requested operation.")
  (:report (lambda (condition stream)
	     (format stream "~&~S has no debug-variable information."
		     (no-debug-variables-debug-function condition)))))

(define-condition lambda-list-unavailable (debug-condition)
  ((debug-function :reader lambda-list-unavailable-debug-function
		   :initarg :debug-function))
  (:documentation
   "The debug-function has no lambda-list since argument debug-variables are
    unavailable.")
  (:report (lambda (condition stream)
	     (format stream "~&~S has no lambda-list information available."
		     (lambda-list-unavailable-debug-function condition)))))

(define-condition invalid-value (debug-condition)
  ((debug-variable :reader invalid-value-debug-variable
		   :initarg :debug-variable)
   (frame :reader invalid-value-frame :initarg :frame))
  (:documentation
   "Indicates a debug-variable has :invalid or :unknown value in a
    particular frame.")
  (:report (lambda (condition stream)
	     (format stream "~&~S has :invalid or :unknown value in ~S."
		     (invalid-value-debug-variable condition)
		     (invalid-value-frame condition)))))

(define-condition ambiguous-variable-name (debug-condition)
  ((name :reader ambiguous-variable-name-name :initarg :name)
   (frame :reader ambiguous-variable-name-frame :initarg :frame))
  (:documentation
   "Indicates a user supplied debug-variable name identifies more than one
    valid variable in a particular frame.")
  (:report (lambda (condition stream)
	     (format stream "~&~S names more than one valid variable in ~S."
		     (ambiguous-variable-name-name condition)
		     (ambiguous-variable-name-frame condition)))))


;;;; Errors and DEBUG-SIGNAL.

#[ Debug-errors

These are programmer errors in the use of the debugging tools' programmers'
interface.  The programmer could have instead used some routine to check
the use of the routine that generated the error.

{condition:di:debug-error}
{condition:di:unhandled-condition}
{condition:di:unknown-code-location}
{condition:di:unknown-debug-variable}
{condition:di:frame-function-mismatch}
]#

;;; The debug-internals code tries to signal all programmer errors as subtypes
;;; of debug-error.  There are calls to ERROR signalling simple-errors, but
;;; these dummy checks in the code and shouldn't come up.
;;;
;;; While under development, this code also signals errors in code branches
;;; that remain unimplemented.

(define-condition debug-error (error) ()
  (:documentation
   "All programmer errors from using the interface for building debugging
    tools inherit from this type."))

(define-condition unhandled-condition (debug-error)
  ((condition :reader unhandled-condition-condition :initarg :condition))
  (:documentation
   "This error results from a signalled debug-condition occurring without
    anyone handling it.")
  (:report (lambda (condition stream)
	     (format stream "~&Unhandled debug-condition:~%~A"
		     (unhandled-condition-condition condition)))))

(define-condition unknown-code-location (debug-error)
  ((code-location :reader unknown-code-location-code-location
		  :initarg :code-location))
  (:documentation
   "Indicates the invalid use of an unknown-code-location.")
  (:report (lambda (condition stream)
	     (format stream "~&Invalid use of an unknown code-location -- ~S."
		     (unknown-code-location-code-location condition)))))

(define-condition unknown-debug-variable (debug-error)
  ((debug-variable :reader unknown-debug-variable-debug-variable
		   :initarg :debug-variable)
   (debug-function :reader unknown-debug-variable-debug-function
		   :initarg :debug-function))
  (:documentation
   "Indicates an attempt to use a debug-variable in conjunction with an
    inappropriate debug-function; for example, checking the variable's
    validity using a code-location in the wrong debug-function will signal
    this error.")
  (:report (lambda (condition stream)
	     (format stream "~&~S not in ~S."
		     (unknown-debug-variable-debug-variable condition)
		     (unknown-debug-variable-debug-function condition)))))

(define-condition invalid-control-stack-pointer (debug-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (fresh-line stream)
	     (write-string "Invalid control stack pointer." stream))))

(define-condition frame-function-mismatch (debug-error)
  ((code-location :reader frame-function-mismatch-code-location
		  :initarg :code-location)
   (frame :reader frame-function-mismatch-frame :initarg :frame)
   (form :reader frame-function-mismatch-form :initarg :form))
  (:documentation
   "Indicates a call to a function returned by `preprocess-for-eval' on a
    frame other than the one for which the function had been prepared.")
  (:report (lambda (condition stream)
	     (format stream
		     "~&Form was preprocessed for ~S,~% but called on ~S:~%  ~S"
		     (frame-function-mismatch-code-location condition)
		     (frame-function-mismatch-frame condition)
		     (frame-function-mismatch-form condition)))))

;;; DEBUG-SIGNAL -- Internal.
;;;
;;; This signals debug-conditions.  If they go unhandled, then signal an
;;; unhandled-condition error.
;;;
;;; ??? Get SIGNAL in the right package!  FIX?
;;;
(defmacro debug-signal (datum &rest arguments)
  `(let ((condition (make-condition ,datum ,@arguments)))
     (signal condition)
     (error 'unhandled-condition :condition condition)))


;;;; Structures.

;;; Most of these structures model information stored in internal data
;;; structures created by the compiler.  Whenever comments preface an object or
;;; type with "compiler", they refer to the internal compiler thing, not to the
;;; object or type with the same name in the "DI" package.

;;;
;;; Debug-variables
;;;

;;; These exist for caching data stored in packed binary form in compiler
;;; debug-functions.  Debug-functions store these.
;;;
(defstruct (debug-variable (:print-function print-debug-variable)
			   (:constructor nil))
  ;;
  ;; String name of variable.
  (name nil :type simple-string)
  ;;
  ;; String name of package.  Nil when variable's name is uninterned.
  (package nil :type (or null simple-string))
  ;;
  ;; Unique integer identification relative to other variables with the same
  ;; name and package.
  (id 0 :type c::index)
  ;;
  ;; Whether the variable always has a valid value.
  (alive-p nil :type boolean))

(defun print-debug-variable (obj str n)
  (declare (ignore n))
  (format str "#<Debug-Variable ~A:~A:~A>"
	  (debug-variable-package obj)
	  (debug-variable-name obj)
	  (debug-variable-id obj)))

(setf (documentation 'debug-variable-name 'function)
  "Return the name of $debug-variable.  The name is the name of the symbol
   used as an identifier when writing the code.")

(setf (documentation 'debug-variable-package 'function)
  "Return the package name of $debug-variable.  This is the package name of
   the symbol used as an identifier when writing the code.")

(setf (documentation 'debug-variable-id 'function)
  "Return the integer that makes $debug-variable's name and package name
   unique with respect to other debug-variables in the same function.")

(defstruct (compiled-debug-variable
	    (:include debug-variable)
	    (:constructor make-compiled-debug-variable
			  (name package id alive-p sc-offset save-sc-offset)))
  ;;
  ;; Storage class and offset.  (unexported).
  (sc-offset nil :type c::sc-offset)
  ;;
  ;; Storage class and offset when saved somewhere.
  (save-sc-offset nil :type (or c::sc-offset null)))

(defstruct (interpreted-debug-variable
	    (:include debug-variable
		      (alive-p t))
	    (:constructor make-interpreted-debug-variable
			  (name package ir1-var)))
  ;;
  ;; This is the IR1 structure that holds information about interpreted vars.
  (ir1-var nil :type c::lambda-var))

;;;
;;; Frames
;;;

;;; These represent call-frames on the stack.
;;;
(defstruct (frame (:constructor nil))
  ;;
  ;; Next frame up.  Null when top frame.
  (up nil :type (or frame null))
  ;;
  ;; Previous frame down.  Nil when the bottom frame.  Before computing the
  ;; next frame down, this slot holds the frame pointer to the control stack
  ;; for the given frame.  This lets us get the next frame down and the
  ;; return-pc for that frame.
  (%down :unparsed :type (or frame (member nil :unparsed)))
  ;;
  ;; Debug-function for function whose call this frame represents.
  (debug-function nil :type debug-function)
  ;;
  ;; Code-location to continue upon return to frame.
  (code-location nil :type code-location)
  ;;
  ;; A-list of catch-tags to code-locations.
  (%catches :unparsed :type (or list (member :unparsed)))
  ;;
  ;; Pointer to frame on control stack.  (unexported)
  ;; When is an interpreted-frame, this is an index into the interpreter's
  ;; stack.
  pointer
  ;;
  ;; This is the frame's number for prompt printing.  Top is zero.
  (number 0 :type index))

(setf (documentation 'frame-up 'function)
  "Return the frame immediately above $frame on the stack.  When $frame is
   the top of the stack, return ().")

(setf (documentation 'frame-debug-function 'function)
  "Return the debug-function for the function whose call $frame
   represents.")

(setf (documentation 'frame-code-location 'function)
  "Return the code-location where $frame's debug-function will continue
   running when program execution returns to this frame.  On interruption
   of this frame, the result could be an unknown code-location.")

(defstruct (compiled-frame
	    (:include frame)
	    (:print-function print-compiled-frame)
	    (:constructor make-compiled-frame
			  (pointer up debug-function code-location number
				   #+gengc saved-state-chain
				   &optional escaped)))
  ;;
  ;; Indicates whether someone interrupted frame.  (unexported).
  ;; If escaped, this is a pointer to the state that was saved when we were
  ;; interrupted.  On the non-gengc system, this is a sigcontext pointer.
  ;; On the gengc system, this is a state pointer from saved-state-chain.
  escaped
  ;;
  ;; List of saps to saved states.  Each time we unwind past an exception,
  ;; we pop the next entry off this list.  When we get to the end of the
  ;; list, there is nothing else on the stack.
  #+gengc (saved-state-chain nil :type list))

(defun print-compiled-frame (obj str n)
  (declare (ignore n))
  (format str "#<Compiled-Frame ~S~:[~;, interrupted~]>"
	  (debug-function-name (frame-debug-function obj))
	  (compiled-frame-escaped obj)))

(defstruct (interpreted-frame
	    (:include frame)
	    (:print-function print-interpreted-frame)
	    (:constructor make-interpreted-frame
			  (pointer up debug-function code-location number
			   real-frame closure)))
  ;;
  ;; This points to the compiled-frame for EVAL:INTERNAL-APPLY-LOOP.
  (real-frame nil :type compiled-frame)
  ;;
  ;; This is the closed over data used by the interpreter.
  (closure nil :type simple-vector))

(defun print-interpreted-frame (obj str n)
  (declare (ignore n))
  (format str "#<Interpreted-Frame ~S>"
	  (debug-function-name (frame-debug-function obj))))

;;;
;;; Debug-functions
;;;

;;; These exist for caching data stored in packed binary form in compiler
;;; debug-functions.  *compiled-debug-functions* maps a c::debug-function to a
;;; debug-function.  There should only be one debug-function in existence for
;;; any function; that is, all code-locations and other objects that reference
;;; debug-functions point to unique objects.  This is due to the overhead in
;;; cached information.
;;;
(defstruct (debug-function (:print-function print-debug-function))
  ;;
  ;; Some representation of the function arguments.  See
  ;; DEBUG-FUNCTION-LAMBDA-LIST.
  ;; NOTE: must parse vars before parsing arg list stuff.
  (%lambda-list :unparsed)
  ;;
  ;; Cached debug-variable information.  (unexported).
  ;; These are sorted by their name.
  (debug-vars :unparsed :type (or simple-vector null (member :unparsed)))
  ;;
  ;; Cached debug-block information.  This is nil when we have tried to parse
  ;; the packed binary info, but none is available.
  (blocks :unparsed :type (or simple-vector null (member :unparsed)))
  ;;
  ;; The actual function if available.
  (%function :unparsed :type (or null function (member :unparsed))))

(defun print-debug-function (obj str n)
  (declare (ignore n))
  (format str "#<~A-Debug-Function ~S>"
	  (etypecase obj
	    (compiled-debug-function "Compiled")
	    (interpreted-debug-function "Interpreted")
	    (bogus-debug-function "Bogus"))
	  (debug-function-name obj)))

(defstruct (compiled-debug-function
	    (:include debug-function)
	    (:constructor %make-compiled-debug-function
			  (compiler-debug-fun component)))
  ;;
  ;; Compiler's dumped debug-function information.  (unexported).
  (compiler-debug-fun nil :type c::compiled-debug-function)
  ;;
  ;; Code object.  (unexported).
  component
  ;;
  ;; The :function-start breakpoint (if any) used to facilitate function
  ;; end breakpoints.
  (end-starter nil :type (or null breakpoint)))

;;; This maps c::compiled-debug-functions to compiled-debug-functions, so we
;;; can get at cached stuff and not duplicate compiled-debug-function
;;; structures.
;;;
(defvar *compiled-debug-functions* (make-hash-table :test #'eq))

;;; MAKE-COMPILED-DEBUG-FUNCTION -- Internal.
;;;
;;; Makes a compiled-debug-function for a c::compiler-debug-function and its
;;; component.  This maps the latter to the former in
;;; *compiled-debug-functions*.  If there already is a compiled-debug-function,
;;; then this returns it from *compiled-debug-functions*.
;;;
(defun make-compiled-debug-function (compiler-debug-fun component)
  (or (gethash compiler-debug-fun *compiled-debug-functions*)
      (setf (gethash compiler-debug-fun *compiled-debug-functions*)
	    (%make-compiled-debug-function compiler-debug-fun component))))

(defstruct (interpreted-debug-function
	    (:include debug-function)
	    (:constructor %make-interpreted-debug-function (ir1-lambda)))
  ;;
  ;; This is the ir1 lambda this debug-function represents.
  (ir1-lambda nil :type c::clambda))

(defstruct (bogus-debug-function
	    (:include debug-function)
	    (:constructor make-bogus-debug-function
			  (%name &aux (%lambda-list nil) (debug-vars nil)
				 (blocks nil) (%function nil))))
  %name)

(defvar *ir1-lambda-debug-function* (make-hash-table :test #'eq))

(defun make-interpreted-debug-function (ir1-lambda)
  (let ((home-lambda (c::lambda-home ir1-lambda)))
    (or (gethash home-lambda *ir1-lambda-debug-function*)
	(setf (gethash home-lambda *ir1-lambda-debug-function*)
	      (%make-interpreted-debug-function home-lambda)))))

;;;
;;; Debug-blocks.
;;;

;;; These exist for caching data stored in packed binary form in compiler
;;; debug-blocks.
;;;
(defstruct (debug-block (:print-function print-debug-block))
  ;;
  ;; Code-locations where execution continues after this block.
  (successors nil :type list)
  ;;
  ;; This indicates whether the block is a special glob of code shared by
  ;; various functions and tucked away elsewhere in a component.  This kind of
  ;; block has no start code-location.  In an interpreted-debug-block, this is
  ;; always nil.  This slot is in all debug-blocks since it is an exported
  ;; interface.
  (elsewhere-p nil :type boolean))

(defun print-debug-block (obj str n)
  (declare (ignore n))
  (format str "#<~A-Debug-Block ~S>"
	  (etypecase obj
	    (compiled-debug-block "Compiled")
	    (interpreted-debug-block "Interpreted"))
	  (debug-block-function-name obj)))

(setf (documentation 'debug-block-successors 'function)
  "Return the list of possible code-locations where execution may continue
   when the basic-block represented by debug-block completes its
   execution.")

(setf (documentation 'debug-block-elsewhere-p 'function)
  "Return whether $debug-block represents elsewhere code.

   This is code the compiler has moved out of a function's code sequence
   for optimization reasons.  Code-locations in these blocks are unsuitable
   for stepping tools, and the first code-location has nothing to do with a
   normal starting location for the block.")

(defstruct (compiled-debug-block (:include debug-block)
				 (:constructor
				  make-compiled-debug-block
				  (code-locations successors elsewhere-p)))
  ;;
  ;; Code-location information for the block.
  (code-locations nil :type simple-vector))

(defstruct (interpreted-debug-block (:include debug-block
					      (elsewhere-p nil))
				    (:constructor %make-interpreted-debug-block
						  (ir1-block)))
  ;;
  ;; This is the IR1 block this debug-block represents.
  (ir1-block nil :type c::cblock)
  ;;
  ;; Code-location information for the block.
  (locations :unparsed :type (or (member :unparsed) simple-vector)))

(defvar *ir1-block-debug-block* (make-hash-table :test #'eq))

;;; MAKE-INTERPRETED-DEBUG-BLOCK -- Internal.
;;;
;;; This makes a debug-block for the interpreter's ir1-block.  If we have it in
;;; the cache, return it.  If we need to make it, then first make debug-blocks
;;; for all the ir1-blocks in ir1-block's home lambda; this makes sure all the
;;; successors of ir1-block have debug-blocks.  We need this to fill in the
;;; resulting debug-block's successors list with debug-blocks, not ir1-blocks.
;;; After making all the possible debug-blocks we'll need to reference, go back
;;; over the list of new debug-blocks and fill in their successor slots with
;;; lists of debug-blocks.  Then look up our argument ir1-block to find its
;;; debug-block since we know we have it now.
;;;
(defun make-interpreted-debug-block (ir1-block)
  (check-type ir1-block c::cblock)
  (let ((res (gethash ir1-block *ir1-block-debug-block*)))
    (or res
	(let ((lambda (c::block-home-lambda ir1-block)))
	  (c::do-blocks (block (c::block-component ir1-block))
	    (when (eq lambda (c::block-home-lambda block))
	      (push (setf (gethash block *ir1-block-debug-block*)
			  (%make-interpreted-debug-block block))
		    res)))
	  (dolist (block res)
	    (let* ((successors nil)
		   (cblock (interpreted-debug-block-ir1-block block))
		   (succ (c::block-succ cblock))
		   (valid-succ
		    (if (and succ
			     (eq (car succ)
				 (c::component-tail
				  (c::block-component cblock))))
			()
			succ)))
	      (dolist (sblock valid-succ)
		(let ((dblock (gethash sblock *ir1-block-debug-block*)))
		  (when dblock
		    (push dblock successors))))
	      (setf (debug-block-successors block) (nreverse successors))))
	  (gethash ir1-block *ir1-block-debug-block*)))))

;;;
;;; Breakpoints.
;;;

;;; This is an internal structure that manages information about a breakpoint
;;; locations.  See *component-breakpoint-offsets*.
;;;
(defstruct (breakpoint-data (:print-function print-breakpoint-data)
			    (:constructor make-breakpoint-data
					  (component offset)))
  ;;
  ;; This is the component in which the breakpoint lies.
  component
  ;;
  ;; This is the byte offset into the component.
  (offset nil :type c::index)
  ;;
  ;; The original instruction replaced by the breakpoint.
  (instruction nil :type (or null (unsigned-byte 32)))
  ;;
  ;; A list of user breakpoints at this location.
  (breakpoints nil :type list))
;;;
(defun print-breakpoint-data (obj str n)
  (declare (ignore n))
  (format str "#<Breakpoint-Data ~S at ~S>"
	  (debug-function-name
	   (debug-function-from-pc (breakpoint-data-component obj)
				   (breakpoint-data-offset obj)))
	  (breakpoint-data-offset obj)))

(defstruct (breakpoint (:print-function print-breakpoint)
		       (:constructor %make-breakpoint
				     (hook-function what kind %info)))
  ;;
  ;; This is the function invoked when execution encounters the breakpoint.  It
  ;; takes a frame, the breakpoint, and optionally a list of values.  Values
  ;; are supplied for :function-end breakpoints as values to return for the
  ;; function containing the breakpoint.  :function-end breakpoint
  ;; hook-functions also take a cookie argument.  See cookie-fun slot.
  (hook-function nil :type function)
  ;;
  ;; Code-location or debug-function.
  (what nil :type (or code-location debug-function))
  ;;
  ;; :code-location, :function-start, or :function-end for that kind of
  ;; breakpoint.  :unknown-return-partner if this is the partner of a
  ;; :code-location breakpoint at an :unknown-return code-location.
  (kind nil :type (member :code-location :function-start :function-end
			  :unknown-return-partner))
  ;;
  ;; Status helps the user and the implementation.
  (status :inactive :type (member :active :inactive :deleted))
  ;;
  ;; This is a backpointer to a breakpoint-data.
  (internal-data nil :type (or null breakpoint-data))
  ;;
  ;; With code-locations whose type is :unknown-return, there are really
  ;; two breakpoints: one at the multiple-value entry point, and one at
  ;; the single-value entry point.  This slot holds the breakpoint for the
  ;; other one, or NIL if this isn't at an :unknown-return code location.
  (unknown-return-partner nil :type (or null breakpoint))
  ;;
  ;; :function-end breakpoints use a breakpoint at the :function-start to
  ;; establish the end breakpoint upon function entry.  We do this by frobbing
  ;; the LRA to jump to a special piece of code that breaks and provides the
  ;; return values for the returnee.  This slot points to the start breakpoint,
  ;; so we can activate, deactivate, and delete it.
  (start-helper nil :type (or null breakpoint))
  ;;
  ;; This is a hook users supply to get a dynamically unique cookie for
  ;; identifying :function-end breakpoint executions.  That is, if there is one
  ;; :function-end breakpoint, but there may be multiple pending calls of its
  ;; function on the stack.  This function takes the cookie, and the
  ;; hook-function takes the cookie too.
  (cookie-fun nil :type (or null function))
  ;;
  ;; This slot users can set with whatever information they find useful.
  %info)
;;;
(defun print-breakpoint (obj str n)
  (declare (ignore n))
  (let ((what (breakpoint-what obj)))
    (format str "#<Breakpoint ~S~:[~;~:*~S~]>"
	    (etypecase what
	      (code-location what)
	      (debug-function (debug-function-name what)))
	    (etypecase what
	      (code-location nil)
	      (debug-function (breakpoint-kind obj))))))

(setf (documentation 'breakpoint-hook-function 'function)
  "Return $breakpoint's function the system calls when execution
   encounters the breakpoint, and it is active.

   This is `setf'able.")

(setf (documentation 'breakpoint-what 'function)
  "Return $breakpoint's what specification.")

(setf (documentation 'breakpoint-kind 'function)
  "Return $breakpoint's kind specification.")

;;;
;;; Code-locations.
;;;

(defstruct (code-location (:print-function print-code-location)
			  (:constructor nil))
  ;;
  ;; This is the debug-function containing code-location.
  (debug-function nil :type debug-function)
  ;;
  ;; This is initially :unsure.  Upon first trying to access an :unparsed slot,
  ;; if the data is unavailable, then this becomes t, and the code-location is
  ;; unknown.  If the data is available, this becomes nil, a known location.
  ;; We can't use a separate type code-location for this since we must return
  ;; code-locations before we can tell whether they're known or unknown.  For
  ;; example, when parsing the stack, we don't want to unpack all the variables
  ;; and blocks just to make frames.
  (%unknown-p :unsure :type (member t nil :unsure))
  ;;
  ;; This is the debug-block containing code-location.
  ;; Possibly toss this out and just find it in the blocks cache in
  ;; debug-function.
  (%debug-block :unparsed :type (or debug-block (member :unparsed)))
  ;;
  ;; This is the number of forms processed by the compiler or loader before
  ;; the top-level form containing this code-location.
  (%tlf-offset :unparsed :type (or c::index (member :unparsed)))
  ;;
  ;; This is the depth-first number of the node that begins code-location
  ;; within its top-level form.
  (%form-number :unparsed :type (or c::index (member :unparsed))))

(defun print-code-location (obj str n)
  (declare (ignore n))
  (format str "#<~A ~S>"
	  (ecase (code-location-unknown-p obj)
	    ((nil) (etypecase obj
		     (compiled-code-location "Compiled-Code-Location")
		     (interpreted-code-location "Interpreted-Code-Location")))
	    ((t) "Unknown-Code-Location"))
	  (debug-function-name (code-location-debug-function obj))))

(setf (documentation 'code-location-debug-function 'function)
  "Return the debug-function representing information about the function
   corresponding to $code-location.")

(defstruct (compiled-code-location
	    (:include code-location)
	    (:constructor make-known-code-location
			  (pc debug-function %tlf-offset %form-number
			      %live-set kind &aux (%unknown-p nil)))
	    (:constructor make-compiled-code-location (pc debug-function)))
  ;;
  ;; This is an index into debug-function's component slot.
  (pc nil :type c::index)
  ;;
  ;; This is a bit-vector indexed by a variable's position in
  ;; DEBUG-FUNCTION-DEBUG-VARS indicating whether the variable has a valid
  ;; value at this code-location.  (unexported).
  (%live-set :unparsed :type (or simple-bit-vector (member :unparsed)))
  ;;
  ;; (unexported)
  ;; To see c::location-kind, do "(kernel:type-expand 'c::location-kind)".
  (kind :unparsed :type (or (member :unparsed) c::location-kind)))

(defstruct (interpreted-code-location
	    (:include code-location
		      (%unknown-p nil))
	    (:constructor make-interpreted-code-location
			  (ir1-node debug-function)))
  ;;
  ;; This is an index into debug-function's component slot.
  (ir1-node nil :type c::node))

;;;
;;; Debug-sources
;;;

#[ Debug-sources

Debug-sources represent how to get back the source for some code.  The
source is either a file (`compile-file' or `load'), a lambda-expression
(`compile', `defun', `defmacro'), or a stream (`compile-from-stream').

When compiling a source, the compiler counts each top-level form it
processes, but when the compiler handles multiple files as one block
compilation, the top-level form count continues past file boundaries.
Therefore `code-location-top-level-form-offset' returns an offset that does
not always start at zero for the code-location's debug-source.  The offset
into a particular source is `code-location-top-level-form-offset' minus
`debug-source-root-number'.

Inside a top-level form, a code-location's form number indicates the subform
corresponding to the code-location.

{function:di:debug-source-from}
{function:di:debug-source-name}
{function:di:debug-source-created}
{function:di:debug-source-compiled}
{function:di:debug-source-root-number}
]#

(proclaim '(inline debug-source-root-number))
;;;
(defun debug-source-root-number (debug-source)
  "Return the number of top-level forms processed by the compiler before
   compiling $debug-source if this is compiled, else zero.

   Return zero even when the source is compiled if the first form in the
   first file compiled in one compilation (this must have a root number of
   zero as it was the first top-level form the compiler saw."
  (c::debug-source-source-root debug-source))

(setf (documentation 'c::debug-source-from 'function)
  "Return an indication of the type of $source.  The following are the
   possible values:
     :file    from a file (obtained by `compile-file' if compiled).
     :lisp    from Lisp (obtained by `compile' if compiled).
     :stream  from a stream other than a file.")

(setf (documentation 'c::debug-source-name 'function)
  "Return the actual source in some sense represented by $debug-source,
   according to the return from `debug-source-from':
     :file    the pathname of the file.
     :lisp    a lambda-expression.
     :stream  some string describing the stream.")

(setf (documentation 'c::debug-source-created 'function)
  "Return the creation time of the source in universal time if the time is
   known, else return ().")

(setf (documentation 'c::debug-source-compiled 'function)
  "Return the compilation time of the source in universal time if the
   source is compiled, else return ().")

(setf (documentation 'c::debug-source-start-positions 'function)
  "Return the file position of each top-level form as a vector if
   `debug-source-from' is :file.  If `debug-source-from' is :lisp or
   :stream, or the file is byte-compiled, then return ().")

(setf (documentation 'c::debug-source-p 'function)
  "Return true if $object is a debug-source, else ().")


;;;; Frames.

#[ Frames

Frames describe a particular call on the stack for a particular thread.  This
is the environment for name resolution, getting arguments and locals, and
returning values.  The stack conceptually grows up, so the top of the stack is
the most recently called function.

`top-frame', `frame-down', `frame-up', and `frame-debug-function' can only
fail when there is absolutely no debug information available.  This can
only happen when someone saved a Lisp image specifying that the system dump
all debugging data.

{function:di:top-frame}
{function:di:frame-down}
{function:di:frame-up}
{function:di:frame-debug-function}
{function:di:frame-code-location}
{function:di:frame-catches}
{function:di:eval-in-frame}
]#

#|
;; FIX Still to be implemented.
{function:di:return-from-frame}
This returns the elements in the list \var{values} as multiple values from
\var{frame} as if the function \var{frame} represents returned these values.
This signals a \code{no-debug-function-returns} condition when \var{frame}'s
debug-function lacks information on returning values.
|#

;;; This is used in FIND-ESCAPE-FRAME and with the bogus components and LRAs
;;; used for :function-end breakpoints.  When a components debug-info slot is
;;; :bogus-lra, then the real-lra-slot contains the real component to continue
;;; executing, as opposed to the bogus component which appeared in some frame's
;;; LRA location.
;;;
(defconstant real-lra-slot vm:code-constants-offset)

;;; These are magically converted by the compiler.
;;;
(defun kernel:current-sp () (kernel:current-sp))
(defun kernel:current-fp () (kernel:current-fp))
(defun kernel:stack-ref (s n) (kernel:stack-ref s n))
(defun kernel:%set-stack-ref (s n value) (kernel:%set-stack-ref s n value))
(defun kernel:function-code-header (fun) (kernel:function-code-header fun))
#-gengc (defun kernel:lra-code-header (lra) (kernel:lra-code-header lra))
(defun kernel:make-lisp-obj (value) (kernel:make-lisp-obj value))
(defun kernel:get-lisp-obj-address (thing) (kernel:get-lisp-obj-address thing))
(defun kernel:function-word-offset (fun) (kernel:function-word-offset fun))
;;;
(defsetf kernel:stack-ref kernel:%set-stack-ref)

;;; DESCRIPTOR-SAP -- internal
;;;
;;; Convert the descriptor into a SAP.  The bits all stay the same, we just
;;; change our notion of what we think they are.
;;;
(declaim (inline descriptor-sap))
(defun descriptor-sap (x)
  (system:int-sap (kernel:get-lisp-obj-address x)))

(declaim (inline cstack-pointer-valid-p))
(defun cstack-pointer-valid-p (x)
  (declare (type system:system-area-pointer x))
  #-:x86
  (and (system:sap< x (kernel:current-sp))
       (system:sap<= #-gengc (alien:alien-sap
			      (alien:extern-alien "control_stack" (* t)))
		     #+gengc (kernel:mutator-control-stack-base)
		     x)
       (zerop (logand (system:sap-int x) #b11)))
  #+:x86 ;; stack grows to low address values
  (and (system:sap>= x (kernel:current-sp))
       (system:sap> (alien:alien-sap
		     (alien:extern-alien "control_stack_end" (* t)))
		    x)
       (zerop (logand (system:sap-int x) #b11))))

#+(or gengc x86)
(alien:def-alien-routine component-ptr-from-pc (system:system-area-pointer)
  (pc system:system-area-pointer))

#+(or gengc x86)
(defun component-from-component-ptr (component-ptr)
  (declare (type system:system-area-pointer component-ptr))
  (kernel:make-lisp-obj
   (logior (system:sap-int component-ptr)
	   vm:other-pointer-type)))

;;;; X86 support.

#+x86
(progn

;;; Note this function should be called with garbage collect inhibited.
(defun compute-lra-data-from-pc (pc)
  (declare (type system-area-pointer pc))
  (let ((component-ptr (component-ptr-from-pc pc)))
    (unless (sap= component-ptr (int-sap #x0))
       (let* ((code (component-from-component-ptr component-ptr))
	      (code-header-len (* (kernel:get-header-data code) vm:word-bytes))
	      (pc-offset (- (sap-int pc)
			    (- (kernel:get-lisp-obj-address code)
			       vm:other-pointer-type)
			    code-header-len)))
;	 (format t "c-lra-fpc ~a ~a ~a~%" pc code pc-offset)
	 (values pc-offset code)))))

(defconstant vm::nargs-offset #.vm::ecx-offset)

;;; Check for a valid return address - it could be any valid C/Lisp
;;; address.
;;;
;;; XX Could be a little smarter.
(declaim (inline ra-pointer-valid-p))
(defun ra-pointer-valid-p (ra)
  (declare (type system:system-area-pointer ra))
  (and
   ;; Not the first page which is unmapped.
   (>= (sys:sap-int ra) 4096)
   ;; Not a Lisp stack pointer.
   (or (sys:sap< ra (kernel:current-sp))
       (sys:sap>= ra (alien:alien-sap
		      (alien:extern-alien "control_stack_end" (* t)))))))

;;; Try to find a valid previous stack.  This is complex on the x86 as it
;;; can jump between C and Lisp frames.  To help find a valid frame it
;;; searches backwards.
;;;
;;; XX Should probably check if it has reached the bottom of the stack.
;;;
;;; XX Should handle interrupted frames, both Lisp and C.  At present it
;;; manages to find a fp trail, see Linux hack below.
;;;
(defun x86-call-context (fp &key (depth 0))
  (declare (type system-area-pointer fp)
	   (fixnum depth))
;  (format t "*CC ~s ~s~%" fp depth)
  (cond
   ((not (cstack-pointer-valid-p fp))
    (format t "Debug invalid fp ~s~%" fp)
    nil)
   (t
    ;; Check the two possible frame pointers.
    (let ((lisp-ocfp (sap-ref-sap fp (- (* (1+ vm::ocfp-save-offset) 4))))
	  (lisp-ra (sap-ref-sap fp (- (* (1+ vm::return-pc-save-offset) 4))))
	  (c-ocfp (sap-ref-sap fp (* 0 vm:word-bytes)))
	  (c-ra (sap-ref-sap fp (* 1 vm:word-bytes))))
      (cond ((and (sap> lisp-ocfp fp) (cstack-pointer-valid-p lisp-ocfp)
		  (ra-pointer-valid-p lisp-ra)
		  (sap> c-ocfp fp) (cstack-pointer-valid-p c-ocfp)
		  (ra-pointer-valid-p c-ra))
;	     (format t "*C Both valid ~s ~s ~s ~s~%"
;		     lisp-ocfp lisp-ra c-ocfp c-ra)
	     ;; Look forward another step to check their validity.
	     (let ((lisp-path-fp (x86-call-context lisp-ocfp
						   :depth (1+ depth)))
		   (c-path-fp (x86-call-context c-ocfp :depth (1+ depth))))
	       (cond ((and lisp-path-fp c-path-fp)
		      ;; Both still seem valid - choose the lisp frame.
		      (when (zerop depth)
			(format t "Debug: Both still valid ~s ~s ~s ~s~%"
				lisp-ocfp lisp-ra c-ocfp c-ra))
		      #+FreeBSD4
		      (if (sap> lisp-ocfp c-ocfp)
			  (values lisp-ra lisp-ocfp)
			  (values c-ra c-ocfp))
		      #-FreeBSD4
		      (values lisp-ra lisp-ocfp))
		     (lisp-path-fp
		      ;; The lisp convention is looking good.
;		      (format t "*C lisp-ocfp ~s ~s~%" lisp-ocfp lisp-ra)
		      (values lisp-ra lisp-ocfp))
		     (c-path-fp
		      ;; The C convention is looking good.
;		      (format t "*C c-ocfp ~s ~s~%" c-ocfp c-ra)
		      (values c-ra c-ocfp))
		     (t
		      ;; Neither seems right?
;		      (format t "Debug: no valid2 fp found ~s ~s~%"
;			      lisp-ocfp c-ocfp)
		      nil))))
	    ((and (sap> lisp-ocfp fp) (cstack-pointer-valid-p lisp-ocfp)
		  (ra-pointer-valid-p lisp-ra))
	     ;; The lisp convention is looking good.
;	     (format t "*C lisp-ocfp ~s ~s~%" lisp-ocfp lisp-ra)
	     (values lisp-ra lisp-ocfp))
	    ((and (sap> c-ocfp fp) (cstack-pointer-valid-p c-ocfp)
		  #-linux (ra-pointer-valid-p c-ra))
	     ;; The C convention is looking good.
;	     (format t "*C c-ocfp ~s ~s~%" c-ocfp c-ra)
	     (values c-ra c-ocfp))
	    (t
;	     (format t "Debug: no valid fp found ~s ~s~%" lisp-ocfp c-ocfp)
	     nil))))))

) ; end progn x86


;;; TOP-FRAME -- Public.
;;;
(defun top-frame ()
  "Return the top frame of the control stack as it was before calling this
   function."
  (multiple-value-bind (fp pc)
      (kernel:%caller-frame-and-pc)
    (possibly-an-interpreted-frame
     (compute-calling-frame (descriptor-sap fp)
			    #-gengc pc #+gengc (descriptor-sap pc)
			    nil)
     nil)))

;;; FLUSH-FRAMES-ABOVE -- public.
;;;
(defun flush-frames-above (frame)
  "Flush all of the frames above $frame, and renumber all the frames below
   $frame."
  (setf (frame-up frame) nil)
  (do ((number 0 (1+ number))
       (frame frame (frame-%down frame)))
      ((not (frame-p frame)))
    (setf (frame-number frame) number)))

;;; FRAME-DOWN -- Public.
;;;
;;; We have to access the old-fp and return-pc out of frame and pass them to
;;; COMPUTE-CALLING-FRAME.
;;;
(defun frame-down (frame)
  "Return the frame immediately below frame on the stack.  When frame is
   the bottom of the stack, return ()."
  (let ((down (frame-%down frame)))
    (if (eq down :unparsed)
	(let* ((real (frame-real-frame frame))
	       (debug-fun (frame-debug-function real)))
	  (setf (frame-%down frame)
		(etypecase debug-fun
		  (compiled-debug-function
		   (let ((c-d-f (compiled-debug-function-compiler-debug-fun
				 debug-fun)))
		     (possibly-an-interpreted-frame
		      (compute-calling-frame
		       (descriptor-sap
			(get-context-value
			 real vm::ocfp-save-offset
			 (c::compiled-debug-function-old-fp c-d-f)))
		       #-gengc
		       (get-context-value
			real vm::lra-save-offset
			(c::compiled-debug-function-return-pc c-d-f))
		       #+gengc
		       (descriptor-sap
			(get-context-value
			 real vm::ra-save-offset
			 (c::compiled-debug-function-return-pc c-d-f)))
		       frame)
		      frame)))
		  (bogus-debug-function
		   (let ((fp (frame-pointer real)))
		     (when (cstack-pointer-valid-p fp)
		       #+x86
		       (multiple-value-bind (ra ofp)
			   (x86-call-context fp)
			 (when (and ra ofp)
			   (compute-calling-frame ofp ra frame)))
		       #-x86
		       (compute-calling-frame
			#-alpha
			(system:sap-ref-sap fp (* vm::ocfp-save-offset
						  vm:word-bytes))
			#+alpha
			(kernel::int-sap
			 (system:sap-ref-32 fp (* vm::ocfp-save-offset
						  vm:word-bytes)))

			#-gengc
			(kernel:stack-ref fp vm::lra-save-offset)
			#+gengc
			(system:sap-ref-sap fp (* vm::ra-save-offset
						  vm:word-bytes))
			frame)))))))
	down)))

;;; GET-CONTEXT-VALUE  --  Internal.
;;;
;;; Get the old FP or return PC out of frame.  Stack-slot is the standard save
;;; location offset on the stack.  Loc is the saved sc-offset describing the
;;; main location.
;;;
#-x86
(defun get-context-value (frame stack-slot loc)
  (declare (type compiled-frame frame) (type unsigned-byte stack-slot)
	   (type c::sc-offset loc))
  (let ((pointer (frame-pointer frame))
	(escaped (compiled-frame-escaped frame)))
    (if escaped
	(sub-access-debug-var-slot pointer loc escaped)
	(kernel:stack-ref pointer stack-slot))))
#+x86
(defun get-context-value (frame stack-slot loc)
  (declare (type compiled-frame frame) (type unsigned-byte stack-slot)
	   (type c::sc-offset loc))
  (let ((pointer (frame-pointer frame))
	(escaped (compiled-frame-escaped frame)))
    (if escaped
	(sub-access-debug-var-slot pointer loc escaped)
	(ecase stack-slot
	  (#.vm::ocfp-save-offset
	   (kernel:stack-ref pointer stack-slot))
	  (#.vm::lra-save-offset
	   (sap-ref-sap pointer (- (* (1+ stack-slot) 4))))))))

;;;
#-x86
(defun (setf get-context-value) (value frame stack-slot loc)
  (declare (type compiled-frame frame) (type unsigned-byte stack-slot)
	   (type c::sc-offset loc))
  (let ((pointer (frame-pointer frame))
	(escaped (compiled-frame-escaped frame)))
    (if escaped
	(sub-set-debug-var-slot pointer loc value escaped)
	(setf (kernel:stack-ref pointer stack-slot) value))))

#+x86
(defun (setf get-context-value) (value frame stack-slot loc)
  (declare (type compiled-frame frame) (type unsigned-byte stack-slot)
	   (type c::sc-offset loc))
  (let ((pointer (frame-pointer frame))
	(escaped (compiled-frame-escaped frame)))
    (if escaped
	(sub-set-debug-var-slot pointer loc value escaped)
	(ecase stack-slot
	  (#.vm::ocfp-save-offset
	   (setf (kernel:stack-ref pointer stack-slot) value))
	  (#.vm::lra-save-offset
	   (setf (sap-ref-sap pointer (- (* (1+ stack-slot) 4))) value))))))

(defvar *debugging-interpreter* nil
  "When set, the debugger foregoes making interpreted-frames, so you can
   debug the functions that manifest the interpreter.")

;;; POSSIBLY-AN-INTERPRETED-FRAME -- Internal.
;;;
;;; This takes a newly computed frame, frame, and the frame above it on the
;;; stack, up-frame, which is possibly nil.  Frame is nil when we hit the
;;; bottom of the control stack.  When frame represents a call to
;;; EVAL::INTERNAL-APPLY-LOOP, we make an interpreted frame to replace frame.
;;; The interpreted frame points to frame.
;;;
(defun possibly-an-interpreted-frame (frame up-frame)
  (if (or (not frame)
	  (not (eq (debug-function-name (frame-debug-function frame))
		   'eval::internal-apply-loop))
	  *debugging-interpreter*
	  (compiled-frame-escaped frame))
      frame
      (flet ((get-var (name location)
	       (let ((vars (di:ambiguous-debug-variables
			    (di:frame-debug-function frame) name)))
		 (when (or (null vars) (> (length vars) 1))
		   (error "Zero or more than one ~A variable in ~
			   EVAL::INTERNAL-APPLY-LOOP?"
			  (string-downcase name)))
		 (if (eq (debug-variable-validity (car vars) location)
			 :valid)
		     (car vars)))))
	(let* ((code-loc (frame-code-location frame))
	       (ptr-var (get-var "FRAME-PTR" code-loc))
	       (node-var (get-var "NODE" code-loc))
	       (closure-var (get-var "CLOSURE" code-loc)))
	  (if (and ptr-var node-var closure-var)
	      (let* ((node (debug-variable-value node-var frame))
		     (d-fun (make-interpreted-debug-function
			     (c::block-home-lambda (c::node-block node)))))
		(make-interpreted-frame
		 (debug-variable-value ptr-var frame)
		 up-frame
		 d-fun
		 (make-interpreted-code-location node d-fun)
		 (frame-number frame)
		 frame
		 (debug-variable-value closure-var frame)))
	      frame)))))

;;; COMPUTE-CALLING-FRAME -- Internal.
;;;
;;; This returns a frame for the one existing in time immediately prior to the
;;; frame referenced by current-fp.  This is current-fp's caller or the next
;;; frame down the control stack.  If there is no down frame, this returns nil
;;; for the bottom of the stack.  Up-frame is the up link for the resulting
;;; frame object, and it is nil when we call this to get the top of the stack.
;;;
;;; The current frame contains the pointer to the temporally previous frame we
;;; want, and the current frame contains the pc at which we will continue
;;; executing upon returning to that previous frame.
;;;
;;; Note: Sometimes LRA is actually a fixnum.  This happens when lisp calls
;;; into C.  In this case, the code object is stored on the stack after the
;;; LRA, and the LRA is the word offset.
;;;
#-(or gengc x86)
(defun compute-calling-frame (caller lra up-frame)
  (declare (type system:system-area-pointer caller))
  (when (cstack-pointer-valid-p caller)
    (multiple-value-bind
	(code pc-offset escaped)
	(if lra
	    (multiple-value-bind
		(word-offset code)
		(if (ext:fixnump lra)
		    (let ((fp (frame-pointer up-frame)))
		      (values lra
			      (kernel:stack-ref fp (1+ vm::lra-save-offset))))
		    (values (kernel:get-header-data lra)
			    (kernel:lra-code-header lra)))
	      (if code
		  (values code
			  (* (1+ (- word-offset (kernel:get-header-data code)))
			     vm:word-bytes)
			  nil)
		  (values :foreign-function
			  0
			  nil)))
	    (find-escaped-frame caller))
      (if (and (kernel:code-component-p code)
	       (eq (kernel:%code-debug-info code) :bogus-lra))
	  (let ((real-lra (kernel:code-header-ref code real-lra-slot)))
	    (compute-calling-frame caller real-lra up-frame))
	  (let ((d-fun (case code
			 (:undefined-function
			  (make-bogus-debug-function
			   "The Undefined Function"))
			 (:foreign-function
			  (make-bogus-debug-function
			   "Foreign function call land"))
			 ((nil)
			  (make-bogus-debug-function
			   "Bogus stack frame"))
			 (t
			  (debug-function-from-pc code pc-offset)))))
	    (make-compiled-frame caller up-frame d-fun
				 (code-location-from-pc d-fun pc-offset
							escaped)
				 (if up-frame (1+ (frame-number up-frame)) 0)
				 escaped))))))

#+x86
(defun compute-calling-frame (caller ra up-frame)
  (declare (type system:system-area-pointer caller ra))
;  (format t "ccf: ~a ~a ~a~%" caller ra up-frame)
  (when (cstack-pointer-valid-p caller)
;    (format t "ccf2~%")
    ;; First check for an escaped frame.
    (multiple-value-bind
	(code pc-offset escaped)
	(find-escaped-frame caller)
	(cond (code
	       ;; If it's escaped it may be a function end breakpoint
	       ;; trap.
;	       (format t "ccf2: escaped ~s ~s~%" code pc-offset)
	       (when (and (kernel:code-component-p code)
			  (eq (kernel:%code-debug-info code) :bogus-lra))
		 ;; If :bogus-lra grab the real lra.
		 (setq pc-offset (kernel:code-header-ref
				  code (1+ real-lra-slot)))
		 (setq code (kernel:code-header-ref code real-lra-slot))
;		 (format t "ccf3 :bogus-lra ~s ~s~%" code pc-offset)
		 (assert code)))
	      (t
	       ;; Not escaped
	       (system:without-gcing
		(multiple-value-setq (pc-offset code)
		  (compute-lra-data-from-pc ra)))
;	       (format t "ccf4 ~s ~s~%" code pc-offset)
	       (unless code
		 (setf code :foreign-function
		       pc-offset 0
		       escaped nil))))

	(let ((d-fun (case code
			   (:undefined-function
			    (make-bogus-debug-function
			     "The Undefined Function"))
			   (:foreign-function
			    (make-bogus-debug-function
			     "Foreign function call land"))
			   ((nil)
			    (make-bogus-debug-function
			     "Bogus stack frame"))
			   (t
			    (debug-function-from-pc code pc-offset)))))
	  (make-compiled-frame caller up-frame d-fun
			       (code-location-from-pc d-fun pc-offset
						      escaped)
			       (if up-frame (1+ (frame-number up-frame)) 0)
			       escaped)))))

#-(or gengc x86)
(defun find-escaped-frame (frame-pointer)
  (declare (type system:system-area-pointer frame-pointer))
  (dotimes (index lisp::*free-interrupt-context-index* (values nil 0 nil))
    (alien:with-alien
	((lisp-interrupt-contexts (array (* unix:sigcontext) nil) :extern))
      (let ((scp (alien:deref lisp-interrupt-contexts index)))
	(when (= (system:sap-int frame-pointer)
		 (vm:sigcontext-register scp vm::cfp-offset))
	  (system:without-gcing
	   (let ((code (code-object-from-bits
			(vm:sigcontext-register scp vm::code-offset))))
	     (when (symbolp code)
	       (return (values code 0 scp)))
	     (let* ((code-header-len (* (kernel:get-header-data code)
					vm:word-bytes))
		    (pc-offset
		     (- (system:sap-int
			 (vm:sigcontext-program-counter scp))
			(- (kernel:get-lisp-obj-address code)
			   vm:other-pointer-type)
			code-header-len)))
	       ;; Check to see if we were executing in a branch delay slot.
	       #+(or pmax sgi) ; pmax only
	       (when (logbitp 31 (alien:slot scp 'mips::sc-cause))
		 (incf pc-offset vm:word-bytes))
	       (unless (<= 0 pc-offset
			   (* (kernel:code-header-ref code
						      vm:code-code-size-slot)
			      vm:word-bytes))
		 ;; We were in an assembly routine.  Therefore, use the LRA as
		 ;; the pc.
		 (setf pc-offset
		       (- (vm:sigcontext-register scp vm::lra-offset)
			  (kernel:get-lisp-obj-address code)
			  code-header-len)))
	       (return
		(if (eq (kernel:%code-debug-info code) :bogus-lra)
		    (let ((real-lra (kernel:code-header-ref code
							    real-lra-slot)))
		      (values (kernel:lra-code-header real-lra)
			      (kernel:get-header-data real-lra)
			      nil))
		    (values code pc-offset scp)))))))))))
#+x86
(defun find-escaped-frame (frame-pointer)
  (declare (type system:system-area-pointer frame-pointer))
  (dotimes (index lisp::*free-interrupt-context-index* (values nil 0 nil))
    (alien:with-alien
	((lisp-interrupt-contexts (array (* unix:sigcontext) nil) :extern))
      (let ((scp (alien:deref lisp-interrupt-contexts index)))
	(when (= (system:sap-int frame-pointer)
		 (vm:sigcontext-register scp vm::cfp-offset))
	  (system:without-gcing
	   (let* ((component-ptr
		   (component-ptr-from-pc (vm:sigcontext-program-counter scp)))
		  (code (if (sap= component-ptr (int-sap #x0))
			    nil
			    (component-from-component-ptr component-ptr))))
	     (when (null code)
	       (return (values code 0 scp)))
	     (let* ((code-header-len (* (kernel:get-header-data code)
					vm:word-bytes))
		    (pc-offset
		     (- (system:sap-int
			 (vm:sigcontext-program-counter scp))
			(- (kernel:get-lisp-obj-address code)
			   vm:other-pointer-type)
			code-header-len)))
	       (unless (<= 0 pc-offset
			   (* (kernel:code-header-ref code
						      vm:code-code-size-slot)
			      vm:word-bytes))
		 ;; We were in an assembly routine.  Therefore, use the LRA as
		 ;; the pc.
		 (format t "** pc-offset ~s not in code obj ~s?~%"
			 pc-offset code))
	       (return
		(values code pc-offset scp))))))))))

;;; CODE-OBJECT-FROM-BITS  --  internal.
;;;
;;; Find the code object corresponding to the object represented by bits and
;;; return it.  We assume bogus functions correspond to the
;;; undefined-function.
;;;
#-gengc
(defun code-object-from-bits (bits)
  (declare (type (unsigned-byte 32) bits))
  (let ((object (kernel:make-lisp-obj bits)))
    (if (functionp object)
	(or (kernel:function-code-header object)
	    :undefined-function)
	(let ((lowtag (kernel:get-lowtag object)))
	  (if (= lowtag vm:other-pointer-type)
	      (let ((type (kernel:get-type object)))
		(cond ((= type vm:code-header-type)
		       object)
		      ((= type vm:return-pc-header-type)
		       (kernel:lra-code-header object))
		      (t
		       nil))))))))

;;; *SAVED-STATE-CHAIN* -- maintained by the C code as a list of saps, each
;;; sap pointing to a saved exception state.
;;;
#+gengc
(declaim (special kernel::*saved-state-chain*))

#+gengc
(defun lookup-trace-table-entry (component pc)
  (declare (type code-component component)
	   (type unsigned-byte pc))
  (let ((tt (system:sap+ (kernel:code-instructions component)
			 (kernel:code-header-ref
			  component
			  vm:code-trace-table-offset-slot)))
	(end (system:sap+ (kernel:code-instructions component)
			  (* (kernel:%code-code-size component)
			     vm:word-bytes))))
    (iterate repeat ((sap tt) (offset 0) (state vm:trace-table-normal))
      (cond ((> offset pc)
	     state)
	    ((system:sap< sap end)
	     (let ((entry (system:sap-ref-16 tt 0)))
	       (repeat (system:sap+ sap 2)
		       (+ offset
			  (ldb (byte c::tt-bits-per-offset
				     c::tt-bits-per-state)
			       entry))
		       (ldb (byte c::tt-bits-per-state 0) entry))))
	    (t
	     vm:trace-table-normal)))))

;;; EXTRACT-INFO-FROM-STATE -- internal.
;;;
;;; Examine the interrupt state and figure out where we were when the interrupt
;;; hit.  Return three values, the debug-function, the pc-offset, and the
;;; control-frame-pointer.
;;;
;;; First, we check to see what component the PC is in the middle of.  There
;;; are a couple interesting cases:
;;;
;;; - no component:
;;;    we were either in one of the C trampoline routines:
;;;     - call_into_lisp
;;;     - call_into_c
;;;     - undefined_tramp
;;;     - closure_tramp
;;;     - function_end_breakpoint
;;;        arn't ever actually in it, because we copy it into a bogus-lra
;;;        component before every actually using it.
;;;    or someone jumped someplace strange, in which case we can't do anything.
;;; - component w/ :ASSEMBLER-ROUTINE for debug-info:
;;;    we are in an assembly routine.  RA will point back into the regular
;;;    component.  In order to find the CFP we need to check the trace table:
;;;     - normal: CFP will hold the correct stack pointer.
;;;     - call-site: OCFP will hold the correct stack pointer.
;;;     - prologue & epilogue: not used
;;; - component w/ :BOGUS-LRA for debug-info:
;;;    we are in the middle of a function-end-breakpoint.
;;; - regular component:
;;;    check the trace table:
;;;     - normal: everything fine: PC & CFP hold the info we want.
;;;     - call-site: same as normal, except use OCFP for the frame pointer.
;;;     - prologue: this frame hasn't been initialized.  Use the caller, who
;;;        can be found by looking at RA and OCFP.
;;;     - epilogue: we are in a world of hurt, because we have trashed the
;;;        current frame and can't reliably find the caller.
;;;
#+gengc
(defun extract-info-from-state (state)
  (declare (type (alien:alien (* unix:sigcontext)) state)
	   (values debug-function unsigned-byte system:system-area-pointer))
  (let* ((pc (vm:sigcontext-program-counter state))
	 (component-ptr (component-ptr-from-pc pc)))
    (if (zerop (system:sap-int component-ptr))
	;; We were in one of the trampoline routines or off in the ether.
	;; ### Need to figure out which one, and do something better.
	(values (make-bogus-debug-function "Trampoline routine")
		0
		(system:int-sap 0))
	;; We have a real component.
	(let* ((component (component-from-component-ptr component-ptr))
	       (pc-offset (- (sap- pc component-ptr)
			     (* (kernel:get-header-data component)
				vm:word-bytes))))
	  (case (kernel:%code-debug-info component)
	    (:assembler-routine
	     (ecase (lookup-trace-table-entry component pc-offset)
	       (#.vm:trace-table-normal
		;; ### Need to do something real.
		(values (make-bogus-debug-function "Assembler routine.")
			0
			(system:int-sap
			 (vm:sigcontext-register state vm::cfp-offset))))
	       (#.vm:trace-table-call-site
		;; ### Need to do something real.
		(values (make-bogus-debug-function "Assembler routine.")
			0
			(system:int-sap
			 (vm:sigcontext-register state vm::ocfp-offset))))
	       (#.vm:trace-table-function-prologue
		(values (make-bogus-debug-function
			 "Function-Prologue in an assembler routine?")
			0
			(system:int-sap 0)))
	       (#.vm:trace-table-function-epilogue
		(values (make-bogus-debug-function
			 "Function-Epilogue in an assembler routine?")
			0
			(system:int-sap 0)))))
	    (:bogus-lra
	     (values (make-bogus-debug-function "Function-end breakpoing")
		     0
		     (system:int-sap 0)))
	    (t
	     (ecase (lookup-trace-table-entry component pc-offset)
	       (#.vm:trace-table-normal
		(values (debug-function-from-pc component pc-offset)
			pc-offset
			(system:int-sap
			 (vm:sigcontext-register state vm::cfp-offset))))
	       (#.vm:trace-table-call-site
		(values (debug-function-from-pc component pc-offset)
			pc-offset
			(system:int-sap
			 (vm:sigcontext-register state vm::ocfp-offset))))
	       (#.vm:trace-table-function-prologue
		#+nil ;; ### Need to do something real.
		(let* ((ra (system:int-sap
			    (vm:sigcontext-register state vm::ra-offset)))
		       (caller-ptr (component-ptr-from-pc ra)))
		  ...)
		(values (make-bogus-debug-function
			 "Interrupted function prologue")
			0
			(system:int-sap 0)))
	       (#.vm:trace-table-function-epilogue
		(values (make-bogus-debug-function
			 "Interrupted function epiloge.")
			0
			(system:int-sap 0))))))))))

;;; COMPUTE-CALLING-FRAME -- GenGC version.
;;;
;;; Compute the frame that called us.  The information we have available is
;;; the old control-frame-pointer and the return-address.
;;;
;;; On the gengc system, there are fewer special cases that compute-calling-
;;; frame needs to take into account.
;;;
#+gengc
(defun compute-calling-frame (ocfp ra up-frame)
  (declare (type system:system-area-pointer ocfp ra))
  (flet ((make-frame (dfun pc-offset &optional (cfp ocfp) state
			   (chain (if up-frame
				      (compiled-frame-saved-state-chain
				       (frame-real-frame up-frame))
				      kernel::*saved-state-chain*)))
	   (make-compiled-frame
	    cfp up-frame dfun
	    (code-location-from-pc dfun pc-offset nil)
	    (if up-frame (1+ (frame-number up-frame)) 0)
	    chain state)))
    (cond
     ((zerop (system:sap-int ocfp))
      ;; If the ocfp is NULL, then we are the first stack frame after an
      ;; exception (or at the start).
      (let ((saved-state-chain (if up-frame
				   (compiled-frame-saved-state-chain
				    (frame-real-frame up-frame))
				   kernel::*saved-state-chain*)))
	(when saved-state-chain
	  ;; Well, there are more saved states.
	  (let ((state
		 (locally
		  (declare (optimize (inhibit-warnings 3)))
		  (alien:sap-alien (car saved-state-chain)
				   (* unix:sigcontext)))))
	    (multiple-value-bind
		(dfun pc-offset cfp)
		(extract-info-from-state state)
	      (make-frame dfun pc-offset cfp state
			  (cdr saved-state-chain)))))))
     ((cstack-pointer-valid-p ocfp)
      ;; The ocfp is valid.  Find the code component that ra points into.
      (let ((component-ptr (component-ptr-from-pc ra)))
	(if (zerop (system:sap-int component-ptr))
	    ;; There isn't a component.  We must have been called from C.
	    (make-frame (make-bogus-debug-function "Foreign function land") 0)
	    ;; There is a component.  Figure out what it is.
	    (let ((component (component-from-component-ptr component-ptr)))
	      ;; ### Should check to see if it is a bogus lra.
	      (let* ((pc-offset
		      (- (sap- ra component-ptr)
			 (* (kernel:get-header-data component)
			    vm:word-bytes))))
		(make-frame (debug-function-from-pc component pc-offset)
			    pc-offset))))))
     (t
      ;; ocfp isn't NULL and isn't valid: we can't tell anything about the
      ;; caller.  This shouldn't happen, and if it does, do something sane.
      (make-frame (make-bogus-debug-function "Bogus stack frame") 0)))))

;;;
;;; Frame utilities.
;;;

;;; DEBUG-FUNCTION-FROM-PC -- Internal.
;;;
;;; This returns a compiled-debug-function for code and pc.  We fetch the
;;; c::debug-info and run down its function-map to get a
;;; c::compiled-debug-function from the pc.  The result only needs to
;;; reference the component, for function constants, and the
;;; c::compiled-debug-function.
;;;
(defun debug-function-from-pc (component pc)
  (let ((info (kernel:%code-debug-info component)))
    (cond
     ((not info)
      (debug-signal 'no-debug-info))
     ((eq info :bogus-lra)
      (make-bogus-debug-function "Function End Breakpoint"))
     (t
      (let* ((function-map (get-debug-info-function-map info))
	     (len (length function-map)))
	(declare (simple-vector function-map))
	(if (= len 1)
	    (make-compiled-debug-function (svref function-map 0) component)
	    (let ((i 1)
		  (elsewhere-p
		   (>= pc (c::compiled-debug-function-elsewhere-pc
			   (svref function-map 0)))))
	      (declare (type c::index i))
	      (loop
		(when (or (= i len)
			  (< pc (if elsewhere-p
				    (c::compiled-debug-function-elsewhere-pc
				     (svref function-map (1+ i)))
				    (svref function-map i))))
		  (return (make-compiled-debug-function
			   (svref function-map (1- i))
			   component)))
		(incf i 2)))))))))

;;; CODE-LOCATION-FROM-PC -- Internal.
;;;
;;; This returns a code-location for the compiled-debug-function, debug-fun,
;;; and the pc into its code vector.  If we stopped at a breakpoint, find
;;; the code-location for that breakpoint.  Otherwise, make an :unsure code
;;; location, so it can be filled in when we figure out what is going on.
;;;
(defun code-location-from-pc (debug-fun pc escaped)
  (or (and (compiled-debug-function-p debug-fun)
	   escaped
	   (let ((data (breakpoint-data
			(compiled-debug-function-component debug-fun)
			pc nil)))
	     (when (and data (breakpoint-data-breakpoints data))
	       (let ((what (breakpoint-what
			    (first (breakpoint-data-breakpoints data)))))
		 (when (compiled-code-location-p what)
		   what)))))
      (make-compiled-code-location pc debug-fun)))

;;; FRAME-CATCHES -- Public.
;;;
(defun frame-catches (frame)
  "Return an a-list mapping catch tags to code-locations.  These are
   code-locations at which execution would continue with $frame as the top
   frame if the corresponding tag was thrown."
  (let ((catch
	 #-gengc (descriptor-sap lisp::*current-catch-block*)
	 #+gengc (kernel:mutator-current-catch-block))
	(res nil)
	(fp (frame-pointer (frame-real-frame frame))))
    (loop
      (when (zerop (sap-int catch)) (return (nreverse res)))
      (when (sap= fp
		  #-alpha
		  (system:sap-ref-sap catch
				      (* vm:catch-block-current-cont-slot
					 vm:word-bytes))
		  #+alpha
		  (kernel::int-sap
		   (system:sap-ref-32 catch
				      (* vm:catch-block-current-cont-slot
					 vm:word-bytes))))
	(let* (#-(or gengc x86)
	       (lra (kernel:stack-ref catch vm:catch-block-entry-pc-slot))
	       #+(or gengc x86)
	       (ra (system:sap-ref-sap
		    catch (* vm:catch-block-entry-pc-slot vm:word-bytes)))
	       #-x86
	       (component
		(kernel:stack-ref catch vm:catch-block-current-code-slot))
	       #+x86
	       (component (component-from-component-ptr
			   (component-ptr-from-pc ra)))
	       (offset
		#-(or gengc x86)
		(* (- (1+ (kernel:get-header-data lra))
		      (kernel:get-header-data component))
		   vm:word-bytes)
		#+gengc
		(+ (- (system:sap-int ra)
		      (kernel:get-lisp-obj-address component)
		      (kernel:get-header-data component))
		   vm:other-pointer-type)
		#+x86
		(- (system:sap-int ra)
		   (- (kernel:get-lisp-obj-address component)
		      vm:other-pointer-type)
		   (* (kernel:get-header-data component) vm:word-bytes))))
	  (push (cons #-x86
		      (kernel:stack-ref catch vm:catch-block-tag-slot)
		      #+x86
		      (kernel:make-lisp-obj
		       (system:sap-ref-32 catch (* vm:catch-block-tag-slot
						   vm:word-bytes)))
		      (make-compiled-code-location
		       offset (frame-debug-function frame)))
		res)))
      (setf catch
	    #-alpha
	    (system:sap-ref-sap catch
				(* vm:catch-block-previous-catch-slot
				   vm:word-bytes))
	    #+alpha
	    (kernel::int-sap
	     (system:sap-ref-32 catch
				(* vm:catch-block-previous-catch-slot
				   vm:word-bytes)))))))

;;; FRAME-REAL-FRAME -- Internal.
;;;
;;; If an interpreted frame, return the real frame, otherwise frame.
;;;
(defun frame-real-frame (frame)
  (etypecase frame
    (compiled-frame frame)
    (interpreted-frame (interpreted-frame-real-frame frame))))


;;;; Debug-functions.

#[ Debug-functions

Debug-functions represent the static information about a function determined at
compile time --- argument and variable storage, their lifetime information,
etc.  The debug-function also contains all the debug-blocks representing
basic-blocks of code, and these contains information about specific
code-locations in a debug-function.

{function:di:do-debug-function-blocks}
{function:di:debug-function-lambda-list}
{function:di:do-debug-function-variables}
{function:di:debug-variable-info-available}
{function:di:debug-function-symbol-variables}
{function:di:ambiguous-debug-variables}
{function:di:preprocess-for-eval}
{function:di:function-debug-function}
{function:di:debug-function-kind}
{function:di:debug-function-function}
{function:di:debug-function-name}
]#

;;; DO-DEBUG-FUNCTION-BLOCKS -- Public.
;;;
(defmacro do-debug-function-blocks ((block-var debug-function &optional result)
				    &body body)
  "Execute $body in a context with $block-var bound to each debug-block in
   $debug-function successively.

   Return the value of executing $result.

   Signal a no-debug-blocks condition when the debug-function lacks
   debug-block information."
  (let ((blocks (gensym))
	(i (gensym)))
    `(let ((,blocks (debug-function-debug-blocks ,debug-function)))
       (declare (simple-vector ,blocks))
       (dotimes (,i (length ,blocks) ,result)
	 (let ((,block-var (svref ,blocks ,i)))
	   ,@body)))))

;;; DO-DEBUG-FUNCTION-VARIABLES -- Public.
;;;
(defmacro do-debug-function-variables ((var debug-function &optional result)
				       &body body)
  "Execute $body in a context with $var bound to each debug-variable in
   $debug-function.

   Return the value of executing $result.

   This may iterate over only some of $debug-function's variables,
   depending on debug policy; for example, possibly the compilation only
   preserved argument information."
  (let ((vars (gensym))
	(i (gensym)))
    `(let ((,vars (debug-function-debug-variables ,debug-function)))
       (declare (type (or null simple-vector) ,vars))
       (if ,vars
	   (dotimes (,i (length ,vars) ,result)
	     (let ((,var (svref ,vars ,i)))
	       ,@body))
	   ,result))))

;;; DEBUG-FUNCTION-FUNCTION -- Public.
;;;
(defun debug-function-function (debug-function)
  "Return the function associated with $debug-function if the function is
   defined and available as a user callable function object, else return
   ()."
  (let ((cached-value (debug-function-%function debug-function)))
    (if (eq cached-value :unparsed)
	(setf (debug-function-%function debug-function)
	      (etypecase debug-function
		(compiled-debug-function
		 (let ((component
			(compiled-debug-function-component debug-function))
		       (start-pc
			(c::compiled-debug-function-start-pc
			 (compiled-debug-function-compiler-debug-fun
			  debug-function))))
		   (do ((entry (kernel:%code-entry-points component)
			       (kernel:%function-next entry)))
		       ((null entry) nil)
		     (when (= start-pc
			      (c::compiled-debug-function-start-pc
			       (compiled-debug-function-compiler-debug-fun
				(function-debug-function entry))))
		       (return entry)))))
		(interpreted-debug-function
		 (c::lambda-eval-info-function
		  (c::leaf-info
		   (interpreted-debug-function-ir1-lambda debug-function))))
		(bogus-debug-function nil)))
	cached-value)))

;;; DEBUG-FUNCTION-NAME -- Public.
;;;
(defun debug-function-name (debug-function)
  "Return the name of the function represented by $debug-function.  This
   may be a string, cons or symbol."
  (etypecase debug-function
    (compiled-debug-function
     (c::compiled-debug-function-name
      (compiled-debug-function-compiler-debug-fun debug-function)))
    (interpreted-debug-function
     (c::lambda-name (interpreted-debug-function-ir1-lambda debug-function)))
    (bogus-debug-function
     (bogus-debug-function-%name debug-function))))

;;; FUNCTION-DEBUG-FUNCTION -- Public.
;;;
(defun function-debug-function (fun)
  "Return a debug-function that represents debug information for function
   $fun."
  (case (get-type fun)
    (#.vm:closure-header-type
     (function-debug-function (%closure-function fun)))
    (#.vm:funcallable-instance-header-type
     (cond ((eval:interpreted-function-p fun)
	    (make-interpreted-debug-function
	     (or (eval::interpreted-function-definition fun)
		 (eval::convert-interpreted-fun fun))))
	   (t
	    (function-debug-function (funcallable-instance-function fun)))))
    ((#.vm:function-header-type #.vm:closure-function-header-type)
      (let* ((name (kernel:%function-name fun))
	     (component (kernel:function-code-header fun))
	     (res (find-if
		   #'(lambda (x)
		       (and (c::compiled-debug-function-p x)
			    (eq (c::compiled-debug-function-name x) name)
			    (eq (c::compiled-debug-function-kind x) nil)))
		   (get-debug-info-function-map
		    (kernel:%code-debug-info component)))))
	(if res
	    (make-compiled-debug-function res component)
	    ;; This used to be the non-interpreted branch, but William wrote it
	    ;; to return the debug-fun of fun's XEP instead of fun's debug-fun.
	    ;; The above code does this more correctly, but it doesn't get or
	    ;; eliminate all appropriate cases.  It mostly works, and probably
	    ;; works for all named functions anyway.
	    (debug-function-from-pc component
				    (* (- (kernel:function-word-offset fun)
					  (kernel:get-header-data component))
				       vm:word-bytes)))))))

;;; DEBUG-FUNCTION-KIND -- Public.
;;;
(defun debug-function-kind (debug-function)
  "Return the kind of function $debug-function represents.  The value is
   one of:

     :optional
         an entry point to an ordinary function.  It handles
         optional fallbacking, parsing keywords, etc.
     :external
         an entry point to an ordinary function.  It checks
         argument values and count and calls the defined function.
     :top-level
         executes one or more random top-level forms
         from a file.
     :cleanup
         represents the cleanup forms in an unwind-protect.
     ()
         Any other kind of function."
  (etypecase debug-function
    (compiled-debug-function
     (c::compiled-debug-function-kind
      (compiled-debug-function-compiler-debug-fun debug-function)))
    (interpreted-debug-function
     (c::lambda-kind (interpreted-debug-function-ir1-lambda debug-function)))
    (bogus-debug-function
     nil)))

;;; DEBUG-VARIABLE-INFO-AVAILABLE -- Public.
;;;
(defun debug-variable-info-available (debug-function)
  "Return whether there is any variable information for $debug-function.

   This is useful for finding out whether there were locals in a
   function or whether there was variable information.

   For example, if `do-debug-function-variables' executes its forms zero
   times, this function can return the reason."
  (debug-function-debug-variables debug-function))

;;; DEBUG-FUNCTION-SYMBOL-VARIABLES -- Public.
;;;
(defun debug-function-symbol-variables (debug-function symbol)
  "Return a list of debug-variables in $debug-function having the same name
   and package as $symbol.

   If $symbol is uninterned, then return a
   list of debug-variables without package names and with the same name as
   symbol.

   The result of this function is limited to the availability of variable
   information in $debug-function; for example, possibly $debug-function
   only knows about its arguments."
  (let ((vars (ambiguous-debug-variables debug-function (symbol-name symbol)))
	(package (if (symbol-package symbol)
		     (package-name (symbol-package symbol)))))
    (delete-if (if (stringp package)
		   #'(lambda (var)
		       (let ((p (debug-variable-package var)))
			 (or (not (stringp p))
			     (string/= p package))))
		   #'(lambda (var)
		       (stringp (debug-variable-package var))))
	       vars)))

;;; AMBIGUOUS-DEBUG-VARIABLES -- Public.
;;;
(defun ambiguous-debug-variables (debug-function name-prefix-string)
  "Return a list of debug-variables in $debug-function, whose names contain
   $name-prefix-string as an intial substring.

   The result of this function is limited to the availability of variable
   information in $debug-function; for example, possibly $debug-function
   only knows about its arguments."
  (declare (simple-string name-prefix-string))
  (let ((variables (debug-function-debug-variables debug-function)))
    (declare (type (or null simple-vector) variables))
    (if variables
	(let* ((len (length variables))
	       (prefix-len (length name-prefix-string))
	       (pos (find-variable name-prefix-string variables len))
	       (res nil))
	  (when pos
	    ;; Find names from pos to variable's len that contain prefix.
	    (do ((i pos (1+ i)))
		((= i len))
	      (let* ((var (svref variables i))
		     (name (debug-variable-name var))
		     (name-len (length name)))
		(declare (simple-string name))
		(when (/= (or (string/= name-prefix-string name
					:end1 prefix-len :end2 name-len)
			      prefix-len)
			  prefix-len)
		  (return))
		(push var res)))
	    (setq res (nreverse res)))
	  res))))

;;; FIND-VARIABLE -- Internal.
;;;
;;; This returns a position in variables for one containing name as an initial
;;; substring.  End is the length of variables if supplied.
;;;
(defun find-variable (name variables &optional end)
  (declare (simple-vector variables)
	   (simple-string name))
  (let ((name-len (length name)))
    (position name variables
	      :test #'(lambda (x y)
			(let* ((y (debug-variable-name y))
			       (y-len (length y)))
			  (declare (simple-string y))
			  (and (>= y-len name-len)
			       (string= x y :end1 name-len :end2 name-len))))
	      :end (or end (length variables)))))

;;; DEBUG-FUNCTION-LAMBDA-LIST -- Public.
;;;
(defun debug-function-lambda-list (debug-function)
  "Return a list representing the lambda-list for $debug-function.  The
   list has the following structure:

      (required-var1 required-var2
       ...
       (:optional var3 suppliedp-var4)
       (:optional var5)
       ...
       (:rest var6) (:rest var7)
       ...
       (:keyword keyword-symbol var8 suppliedp-var9)
       (:keyword keyword-symbol var10)
       ...
      )

   Each \"vari\" is a debug-variable or the symbol :deleted it is
   unreferenced in $DEBUG-FUNCTION.  This signals a lambda-list-unavaliable
   condition if argument list information is missing."
  (etypecase debug-function
    (compiled-debug-function
     (compiled-debug-function-lambda-list debug-function))
    (interpreted-debug-function
     (interpreted-debug-function-lambda-list debug-function))
    (bogus-debug-function
     ())))

;;; INTERPRETED-DEBUG-FUNCTION-LAMBDA-LIST -- Internal.
;;;
;;; The hard part is when the lambda-list is unparsed.  If it is unparsed,
;;; and all the arguments are required, this is still pretty easy; just
;;; whip the appropriate debug-variables into a list.  Otherwise, we have
;;; to pick out the funny arguments including any suppliedp variables.  In
;;; this situation, the ir1-lambda is an external entry point that takes
;;; arguments users really pass in.  It looks at those and computes defaults
;;; and suppliedp variables, ultimately passing everything defined as a
;;; a parameter to the real function as final arguments.  If this has to
;;; compute the lambda list, it caches it in debug-function.
;;;
(defun interpreted-debug-function-lambda-list (debug-function)
  (let ((lambda-list (debug-function-%lambda-list debug-function))
	(debug-vars (debug-function-debug-variables debug-function))
	(ir1-lambda (interpreted-debug-function-ir1-lambda debug-function))
	(res nil))
    (if (eq lambda-list :unparsed)
	(flet ((frob (v debug-vars)
		 (if (c::lambda-var-refs v)
		     (find v debug-vars
			   :key #'interpreted-debug-variable-ir1-var)
		     :deleted)))
	  (let ((xep-args (c::lambda-optional-dispatch ir1-lambda)))
	    (if (and xep-args
		     (eq (c::optional-dispatch-main-entry xep-args) ir1-lambda))
		;;
		;; There are rest, optional, keyword, and suppliedp vars.
		(let ((final-args (c::lambda-vars ir1-lambda)))
		  (dolist (xep-arg (c::optional-dispatch-arglist xep-args))
		    (let ((info (c::lambda-var-arg-info xep-arg))
			  (final-arg (pop final-args)))
		      (cond (info
			     (case (c::arg-info-kind info)
			       (:required
				(push (frob final-arg debug-vars) res))
			       (:keyword
				(push (list :keyword
					    (c::arg-info-keyword info)
					    (frob final-arg debug-vars))
				      res))
			       (:rest
				(push (list :rest (frob final-arg debug-vars))
				      res))
			       (:optional
				(push (list :optional
					    (frob final-arg debug-vars))
				      res)))
			     (when (c::arg-info-supplied-p info)
			       (nconc
				(car res)
				(list (frob (pop final-args) debug-vars)))))
			    (t
			     (push (frob final-arg debug-vars) res)))))
		  (setf (debug-function-%lambda-list debug-function)
			(nreverse res)))
		;;
		;; All required args, so return them in a list.
		(dolist (v (c::lambda-vars ir1-lambda)
			   (setf (debug-function-%lambda-list debug-function)
				 (nreverse res)))
		  (push (frob v debug-vars) res)))))
	;;
	;; Everything's unparsed and cached, so return it.
	lambda-list)))

;;; COMPILED-DEBUG-FUNCTION-LAMBDA-LIST -- Internal.
;;;
;;; If this has to compute the lambda list, it caches it in debug-function.
;;;
(defun compiled-debug-function-lambda-list (debug-function)
  (let ((lambda-list (debug-function-%lambda-list debug-function)))
    (cond ((eq lambda-list :unparsed)
	   (multiple-value-bind
	       (args argsp)
	       (parse-compiled-debug-function-lambda-list debug-function)
	     (setf (debug-function-%lambda-list debug-function) args)
	     (if argsp
		 args
		 (debug-signal 'lambda-list-unavailable
			       :debug-function debug-function))))
	  (lambda-list)
	  ((bogus-debug-function-p debug-function)
	   nil)
	  ((c::compiled-debug-function-arguments
	    (compiled-debug-function-compiler-debug-fun
	     debug-function))
	   ;; If the packed information is there (whether empty or not) as
	   ;; opposed to being nil, then returned our cached value (nil).
	   nil)
	  (t
	   ;; Our cached value is nil, and the packed lambda-list information
	   ;; is nil, so we don't have anything available.
	   (debug-signal 'lambda-list-unavailable
			 :debug-function debug-function)))))

;;; PARSE-COMPILED-DEBUG-FUNCTION-LAMBDA-LIST -- Internal.
;;;
;;; COMPILED-DEBUG-FUNCTION-LAMBDA-LIST calls this when a
;;; compiled-debug-function has no lambda-list information cached.  It returns
;;; the lambda-list as the first value and whether there was any argument
;;; information as the second value.  Therefore, nil and t means there were no
;;; arguments, but nil and nil means there was no argument information.
;;;
(defun parse-compiled-debug-function-lambda-list (debug-function)
  (let ((args (c::compiled-debug-function-arguments
	       (compiled-debug-function-compiler-debug-fun
		debug-function))))
    (cond
     ((not args)
      (values nil nil))
     ((eq args :minimal)
      (values (coerce (debug-function-debug-variables debug-function) 'list)
	      t))
     (t
      (let ((vars (debug-function-debug-variables debug-function))
	    (i 0)
	    (len (length args))
	    (res nil)
	    (optionalp nil))
	(declare (type (or null simple-vector) vars))
	(loop
	  (when (>= i len) (return))
	  (let ((ele (aref args i)))
	    (cond
	     ((symbolp ele)
	      (case ele
		(c::deleted
		 ;; Deleted required arg at beginning of args array.
		 (push :deleted res))
		(c::optional-args
		 (setf optionalp t))
		(c::supplied-p
		 ;; supplied-p var immediately following keyword or optional.
		 ;; Stick the extra var in the result element representing
		 ;; the keyword or optional, which is the previous one.
		 (nconc (car res)
			(list (compiled-debug-function-lambda-list-var
			       args (incf i) vars))))
		(c::rest-arg
		 (push (list :rest
			     (compiled-debug-function-lambda-list-var
			      args (incf i) vars))
		       res))
		(c::more-arg
		 ;; Just ignore the fact that the next two args are the
		 ;; more arg context and count, and act like they are
		 ;; regular arguments.
		 nil)
		(t
		 ;; Keyword arg.
		 (push (list :keyword
			     ele
			     (compiled-debug-function-lambda-list-var
			      args (incf i) vars))
		       res))))
	     (optionalp
	      ;; We saw an optional marker, so the following non-symbols are
	      ;; indexes indicating optional variables.
	      (push (list :optional (svref vars ele)) res))
	     (t
	      ;; Required arg at beginning of args array.
	      (push (svref vars ele) res))))
	  (incf i))
	(values (nreverse res) t))))))

;;; COMPILED-DEBUG-FUNCTION-LAMBDA-LIST-VAR -- Internal
;;;
;;; Used in COMPILED-DEBUG-FUNCTION-LAMBDA-LIST.
;;;
(defun compiled-debug-function-lambda-list-var (args i vars)
  (declare (type (simple-array * (*)) args)
	   (simple-vector vars))
  (let ((ele (aref args i)))
    (cond ((not (symbolp ele)) (svref vars ele))
	  ((eq ele 'c::deleted) :deleted)
	  (t (error "Malformed arguments description.")))))

;;; COMPILED-DEBUG-FUNCTION-DEBUG-INFO -- Internal.
;;;
(defun compiled-debug-function-debug-info (debug-fun)
  (kernel:%code-debug-info (compiled-debug-function-component debug-fun)))


;;;; Unpacking variable and basic block data.

(defvar *parsing-buffer*
  (make-array 20 :adjustable t :fill-pointer t))
(defvar *other-parsing-buffer*
  (make-array 20 :adjustable t :fill-pointer t))

;;; WITH-PARSING-BUFFER -- Internal.
;;;
;;; PARSE-DEBUG-BLOCKS, PARSE-DEBUG-VARIABLES and UNCOMPACT-FUNCTION-MAP use
;;; this to unpack binary encoded information.  It returns the values returned
;;; by the last form in body.
;;;
;;; This binds buffer-var to *parsing-buffer*, makes sure it starts at element
;;; zero, and makes sure if we unwind, we nil out any set elements for GC
;;; purposes.
;;;
;;; This also binds other-var to *other-parsing-buffer* when it is supplied,
;;; making sure it starts at element zero and that we nil out any elements if
;;; we unwind.
;;;
;;; This defines the local macro RESULT that takes a buffer, copies its
;;; elements to a resulting simple-vector, nil's out elements, and restarts
;;; the buffer at element zero.  RESULT returns the simple-vector.
;;;
(eval-when (compile eval)
(defmacro with-parsing-buffer ((buffer-var &optional other-var) &body body)
  (let ((len (gensym))
	(res (gensym)))
    `(unwind-protect
	 (let ((,buffer-var *parsing-buffer*)
	       ,@(if other-var `((,other-var *other-parsing-buffer*))))
	   (setf (fill-pointer ,buffer-var) 0)
	   ,@(if other-var `((setf (fill-pointer ,other-var) 0)))
	   (macrolet ((result (buf)
			`(let* ((,',len (length ,buf))
				(,',res (make-array ,',len)))
			   (replace ,',res ,buf :end1 ,',len :end2 ,',len)
			   (fill ,buf nil :end ,',len)
			   (setf (fill-pointer ,buf) 0)
			   ,',res)))
	     ,@body))
     (fill *parsing-buffer* nil)
     ,@(if other-var `((fill *other-parsing-buffer* nil))))))
) ;eval-when

;;; DEBUG-FUNCTION-DEBUG-BLOCKS -- Internal.
;;;
;;; The argument is a debug internals structure.  This returns the debug-blocks
;;; for debug-function, regardless of whether we have unpacked them yet.  It
;;; signals a no-debug-blocks condition if it can't return the blocks.
;;;
(defun debug-function-debug-blocks (debug-function)
  (let ((blocks (debug-function-blocks debug-function)))
    (cond ((eq blocks :unparsed)
	   (setf (debug-function-blocks debug-function)
		 (parse-debug-blocks debug-function))
	   (unless (debug-function-blocks debug-function)
	     (debug-signal 'no-debug-blocks
			   :debug-function debug-function))
	   (debug-function-blocks debug-function))
	  (blocks)
	  (t
	   (debug-signal 'no-debug-blocks
			 :debug-function debug-function)))))

;;; PARSE-DEBUG-BLOCKS -- Internal.
;;;
;;; This returns a simple-vector of debug-blocks or nil.  Nil indicates there
;;; was no basic block information.
;;;
(defun parse-debug-blocks (debug-function)
  (etypecase debug-function
    (compiled-debug-function
     (parse-compiled-debug-blocks debug-function))
    (bogus-debug-function
     (debug-signal 'no-debug-blocks :debug-function debug-function))
    (interpreted-debug-function
     (parse-interpreted-debug-blocks debug-function))))

;;; PARSE-COMPILED-DEBUG-BLOCKS -- Internal.
;;;
;;; This does some of the work of PARSE-DEBUG-BLOCKS.
;;;
(defun parse-compiled-debug-blocks (debug-function)
  (let* ((debug-fun (compiled-debug-function-compiler-debug-fun debug-function))
	 (var-count (length (debug-function-debug-variables debug-function)))
	 (blocks (c::compiled-debug-function-blocks debug-fun))
	 ;; 8 is a hard-wired constant in the compiler for the element size of
	 ;; the packed binary representation of the blocks data.
	 (live-set-len (ceiling var-count 8))
	 (tlf-number (c::compiled-debug-function-tlf-number debug-fun)))
    (or blocks (return-from parse-compiled-debug-blocks nil))
    (macrolet ((aref+ (a i) `(prog1 (aref ,a ,i) (incf ,i))))
      (with-parsing-buffer (blocks-buffer locations-buffer)
	(let ((i 0)
	      (len (length blocks))
	      (last-pc 0))
	  (loop
	    (when (>= i len) (return))
	    (let ((succ-and-flags (aref+ blocks i))
		  (successors nil))
	      (declare (type (unsigned-byte 8) succ-and-flags)
		       (list successors))
	      (dotimes (k (ldb c::compiled-debug-block-nsucc-byte
			       succ-and-flags))
		(push (c::read-var-integer blocks i) successors))
	      (let* ((locations
		      (dotimes (k (c::read-var-integer blocks i)
				  (result locations-buffer))
			(let ((kind (svref c::compiled-code-location-kinds
					   (aref+ blocks i)))
			      (pc (+ last-pc (c::read-var-integer blocks i)))
			      (tlf-offset (or tlf-number
					      (c::read-var-integer blocks i)))
			      (form-number (c::read-var-integer blocks i))
			      (live-set (c::read-packed-bit-vector
					 live-set-len blocks i)))
			  (vector-push-extend (make-known-code-location
					       pc debug-function tlf-offset
					       form-number live-set kind)
					      locations-buffer)
			  (setf last-pc pc))))
		     (block (make-compiled-debug-block
			     locations successors
			     (not (zerop (logand
					  c::compiled-debug-block-elsewhere-p
					  succ-and-flags))))))
		(vector-push-extend block blocks-buffer)
		(dotimes (k (length locations))
		  (setf (code-location-%debug-block (svref locations k))
			block))))))
	(let ((res (result blocks-buffer)))
	  (declare (simple-vector res))
	  (dotimes (i (length res))
	    (let* ((block (svref res i))
		   (succs nil))
	      (dolist (ele (debug-block-successors block))
		(push (svref res ele) succs))
	      (setf (debug-block-successors block) succs)))
	  res)))))

;;; PARSE-INTERPRETED-DEBUG-BLOCKS -- Internal.
;;;
;;; This does some of the work of PARSE-DEBUG-BLOCKS.
;;;
(defun parse-interpreted-debug-blocks (debug-function)
  (let ((ir1-lambda (interpreted-debug-function-ir1-lambda debug-function)))
    (with-parsing-buffer (buffer)
      (c::do-blocks (block (c::block-component
			    (c::node-block (c::lambda-bind ir1-lambda))))
	(when (eq ir1-lambda (c::block-home-lambda block))
	  (vector-push-extend (make-interpreted-debug-block block) buffer)))
      (result buffer))))

;;; DEBUG-FUNCTION-DEBUG-VARIABLES -- Internal.
;;;
;;; The argument is a debug internals structure.  This returns nil if there is
;;; no variable information.  It returns an empty simple-vector if there were
;;; no locals in the function.  Otherwise it returns a simple-vector of
;;; debug-variables.
;;;
(defun debug-function-debug-variables (debug-function)
  (let ((vars (debug-function-debug-vars debug-function)))
    (if (eq vars :unparsed)
	(setf (debug-function-debug-vars debug-function)
	      (etypecase debug-function
		(compiled-debug-function
		 (parse-compiled-debug-variables debug-function))
		(bogus-debug-function nil)
		(interpreted-debug-function
		 (parse-interpreted-debug-variables debug-function))))
	vars)))

;;; PARSE-INTERPRETED-DEBUG-VARIABLES -- Internal.
;;;
;;; This grabs all the variables from debug-fun's ir1-lambda, from the IR1
;;; lambda vars, and all of it's LET's.  Each LET is an IR1 lambda.  For each
;;; variable, we make an interpreted-debug-variable.  We then SORT all the
;;; variables by name.  Then we go through, and for any duplicated names we
;;; distinguish the interpreted-debug-variables by setting their id slots to a
;;; distinct number.
;;;
(defun parse-interpreted-debug-variables (debug-fun)
  (let* ((ir1-lambda (interpreted-debug-function-ir1-lambda debug-fun))
	 (vars (flet ((frob (ir1-lambda buf)
			(dolist (v (c::lambda-vars ir1-lambda))
			  (vector-push-extend
			   (let* ((id (c::leaf-name v))
				  (pkg (symbol-package id)))
			     (make-interpreted-debug-variable
			      (symbol-name id)
			      (when pkg (package-name pkg))
			      v))
			   buf))))
		 (with-parsing-buffer (buf)
		   (frob ir1-lambda buf)
		   (dolist (let-lambda (c::lambda-lets ir1-lambda))
		     (frob let-lambda buf))
		   (result buf)))))
    (declare (simple-vector vars))
    (sort vars #'string< :key #'debug-variable-name)
    (let ((len (length vars)))
      (when (> len 1)
	(let ((i 0)
	      (j 1))
	  (block PUNT
	    (loop
	      (let* ((var-i (svref vars i))
		     (var-j (svref vars j))
		     (name (debug-variable-name var-i)))
		(when (string= name (debug-variable-name var-j))
		  (let ((count 1))
		    (loop
		      (setf (debug-variable-id var-j) count)
		      (when (= (incf j) len) (return-from PUNT))
		      (setf var-j (svref vars j))
		      (when (string/= name (debug-variable-name var-j))
			(return))
		      (incf count))))
		(setf i j)
		(incf j)
		(when (= j len) (return))))))))
    vars))

;;; ASSIGN-MINIMAL-VAR-NAMES -- Internal.
;;;
;;; Vars is the parsed variables for a minimal debug function.  We need to
;;; assign names of the form ARG-NNN.  We must pad with leading zeros, since
;;; the arguments must be in alphabetical order.
;;;
(defun assign-minimal-var-names (vars)
  (declare (simple-vector vars))
  (let* ((len (length vars))
	 (width (length (format nil "~D" (1- len)))))
    (dotimes (i len)
      (setf (compiled-debug-variable-name (svref vars i))
	    (format nil "ARG-~V,'0D" width i)))))

;;; PARSE-COMPILED-DEBUG-VARIABLES -- Internal.
;;;
;;; This parses the packed binary representation of debug-variables from
;;; debug-function's c::compiled-debug-function.
;;;
(defun parse-compiled-debug-variables (debug-function)
  (let* ((debug-fun (compiled-debug-function-compiler-debug-fun debug-function))
	 (packed-vars (c::compiled-debug-function-variables debug-fun))
	 (default-package (c::compiled-debug-info-package
			   (compiled-debug-function-debug-info debug-function)))
	 (args-minimal (eq (c::compiled-debug-function-arguments debug-fun)
			   :minimal)))
    (or packed-vars
	(return-from parse-compiled-debug-variables nil))
    (if (zerop (length packed-vars))
	;; Return a simple-vector not whatever packed-vars may be.
	(return-from parse-compiled-debug-variables '#()))
    (let ((i 0)
	  (len (length packed-vars)))
      (with-parsing-buffer (buffer)
	(loop
	  ;; The routines in the "C" package are macros that advance the
	  ;; index.
	  (let* ((flags (prog1 (aref packed-vars i) (incf i)))
		 (minimal (logtest c::compiled-debug-variable-minimal-p flags))
		 (deleted (logtest c::compiled-debug-variable-deleted-p flags))
		 (name (if minimal "" (c::read-var-string packed-vars i)))
		 (package (cond
			   (minimal default-package)
			   ((logtest c::compiled-debug-variable-packaged
				     flags)
			    (c::read-var-string packed-vars i))
			   ((logtest c::compiled-debug-variable-uninterned
				     flags)
			    nil)
			   (t
			    default-package)))
		  (id (if (logtest c::compiled-debug-variable-id-p flags)
			  (c::read-var-integer packed-vars i)
			  0))
		  (sc-offset
		   (if deleted 0 (c::read-var-integer packed-vars i)))
		  (save-sc-offset
		   (if (logtest c::compiled-debug-variable-save-loc-p flags)
		       (c::read-var-integer packed-vars i)
		       nil)))
	    (assert (not (and args-minimal (not minimal))))
	    (vector-push-extend
	     (make-compiled-debug-variable
	      name package id
	      (logtest c::compiled-debug-variable-environment-live flags)
	      sc-offset save-sc-offset)
	     buffer))
	  (when (>= i len) (return)))
	(let ((res (result buffer)))
	  (when args-minimal
	    (assign-minimal-var-names res))
	  res)))))


;;;; Unpacking minimal debug functions.

(eval-when (compile eval)

;;; MAKE-UNCOMPACTED-DEBUG-FUN -- Internal.
;;;
;;; Sleazoid "macro" to keep our indentation sane in UNCOMPACT-FUNCTION-MAP.
;;;
(defmacro make-uncompacted-debug-fun ()
  '(c::make-compiled-debug-function
    :name
    (let ((base (ecase (ldb c::minimal-debug-function-name-style-byte
			    options)
		  (#.c::minimal-debug-function-name-symbol
		   (intern (c::read-var-string map i)
			   (c::compiled-debug-info-package info)))
		  (#.c::minimal-debug-function-name-packaged
		   (let ((pkg (c::read-var-string map i)))
		     (intern (c::read-var-string map i) pkg)))
		  (#.c::minimal-debug-function-name-uninterned
		   (make-symbol (c::read-var-string map i)))
		  (#.c::minimal-debug-function-name-component
		   (c::compiled-debug-info-name info)))))
      (if (logtest flags c::minimal-debug-function-setf-bit)
	  `(setf ,base)
	  base))
    :kind (svref c::minimal-debug-function-kinds
		 (ldb c::minimal-debug-function-kind-byte options))
    :variables
    (when vars-p
      (let ((len (c::read-var-integer map i)))
	(prog1 (subseq map i (+ i len))
	  (incf i len))))
    :arguments (when vars-p :minimal)
    :returns
    (ecase (ldb c::minimal-debug-function-returns-byte options)
      (#.c::minimal-debug-function-returns-standard
       :standard)
      (#.c::minimal-debug-function-returns-fixed
       :fixed)
      (#.c::minimal-debug-function-returns-specified
       (with-parsing-buffer (buf)
	 (dotimes (idx (c::read-var-integer map i))
	   (vector-push-extend (c::read-var-integer map i) buf))
	 (result buf))))
    :return-pc (c::read-var-integer map i)
    :old-fp (c::read-var-integer map i)
    :nfp (if (logtest flags c::minimal-debug-function-nfp-bit)
	     (c::read-var-integer map i))
    :start-pc
    (progn
      (setq code-start-pc (+ code-start-pc (c::read-var-integer map i)))
      (+ code-start-pc (c::read-var-integer map i)))
    :elsewhere-pc
    (setq elsewhere-pc (+ elsewhere-pc (c::read-var-integer map i)))))

) ;EVAL-WHEN (compile eval)

;;; UNCOMPACT-FUNCTION-MAP  --  Internal
;;;
;;; Return a normal function map derived from a minimal debug info function
;;; map.  This involves looping parsing minimal-debug-functions and then
;;; building a vector out of them.
;;;
(defun uncompact-function-map (info)
  (declare (type c::compiled-debug-info info))
  (let* ((map (c::compiled-debug-info-function-map info))
	 (i 0)
	 (len (length map))
	 (code-start-pc 0)
	 (elsewhere-pc 0))
    (declare (type (simple-array (unsigned-byte 8) (*)) map))
    (ext:collect ((res))
      (loop
	(when (= i len) (return))
	(let* ((options (prog1 (aref map i) (incf i)))
	       (flags (prog1 (aref map i) (incf i)))
	       (vars-p (logtest flags c::minimal-debug-function-variables-bit))
	       (dfun (make-uncompacted-debug-fun)))
	  (res code-start-pc)
	  (res dfun)))

      (coerce (cdr (res)) 'simple-vector))))

;;; This variable maps minimal debug-info function maps to an unpacked version
;;; thereof.
;;;
(defvar *uncompacted-function-maps* (make-hash-table :test #'eq))

;;; GET-DEBUG-INFO-FUNCTION-MAP  --  Internal
;;;
;;; Return a function-map for a given compiled-debug-info object.  If the
;;; info is minimal, and has not been parsed, then parse it.
;;;
(defun get-debug-info-function-map (info)
  (declare (type c::compiled-debug-info info))
  (let ((map (c::compiled-debug-info-function-map info)))
    (if (simple-vector-p map)
	map
	(or (gethash map *uncompacted-function-maps*)
	    (setf (gethash map *uncompacted-function-maps*)
		  (uncompact-function-map info))))))


;;;; Code-locations.

#[ Code-locations

Code-locations represent places in functions where the system has correct
information about the function's environment and where interesting operations
can occur --- asking for a local variable's value, setting breakpoints,
evaluating forms within the function's environment, etc.

Sometimes the interface returns unknown code-locations.  These represent places
in functions, but there is no debug information associated with them.  Some
operations accept these since they may succeed even with missing debug data.
These operations' argument is named basic-code-location indicating they
take known and unknown code-locations.  If an operation names its argument
code-location, and you supply an unknown one, it will signal an error.
For example, frame-code-location may return an unknown code-location if
someone interrupted Lisp in the given frame.  The system knows where execution
will continue, but this place in the code may not be a place for which the
compiler dumped debug information.

{function:di:code-location-debug-function}
{function:di:code-location-debug-block}
{function:di:code-location-top-level-form-offset}
{function:di:code-location-form-number}
{function:di:code-location-debug-source}
{function:di:code-location-unknown-p}
{function:di:code-location=}
]#

;;; CODE-LOCATION-UNKNOWN-P -- Public.
;;;
;;; If we're sure of whether code-location is known, return t or nil.  If we're
;;; :unsure, then try to fill in the code-location's slots.  This determines
;;; whether there is any debug-block information, and if code-location is
;;; known.
;;;
;;; ??? IF this conses closures every time it's called, then break off the
;;; :unsure part to get the HANDLER-CASE into another function.   FIX?
;;;
(defun code-location-unknown-p (basic-code-location)
  "Return () if the code-location is known, else return t."
  (ecase (code-location-%unknown-p basic-code-location)
    ((t) t)
    ((nil) nil)
    (:unsure
     (setf (code-location-%unknown-p basic-code-location)
	   (handler-case (not (fill-in-code-location basic-code-location))
	     (no-debug-blocks () t))))))

;;; CODE-LOCATION-DEBUG-BLOCK -- Public.
;;;
(defun code-location-debug-block (basic-code-location)
  "Return the debug-block containing $basic-code-location if it is
   available, else signal a no-debug-blocks condition.

   Some debug policies withhold debug-block information."
  (let ((block (code-location-%debug-block basic-code-location)))
    (if (eq block :unparsed)
	(etypecase basic-code-location
	  (compiled-code-location
	   (compute-compiled-code-location-debug-block basic-code-location))
	  (interpreted-code-location
	   (setf (code-location-%debug-block basic-code-location)
		 (make-interpreted-debug-block
		  (c::node-block
		   (interpreted-code-location-ir1-node basic-code-location))))))
	block)))

;;; COMPUTE-COMPILED-CODE-LOCATION-DEBUG-BLOCK -- Internal.
;;;
;;; This stores and returns basic-code-location's debug-block.  It determines
;;; the correct one using the code-location's pc.  This uses
;;; DEBUG-FUNCTION-DEBUG-BLOCKS to return the cached block information or
;;; signal a 'no-debug-blocks condition.  The blocks are sorted by their first
;;; code-location's pc, in ascending order.  Therefore, as soon as we find a
;;; block that starts with a pc greater than basic-code-location's pc, we know
;;; the previous block contains the pc.  If we get to the last block, then the
;;; code-location is either in the second to last block or the last block, and
;;; we have to be careful in determining this since the last block could be
;;; random code at the end of the function.  We have to check for the last
;;; block being random code first to see how to compare the code-location's pc.
;;;
(defun compute-compiled-code-location-debug-block (basic-code-location)
  (let* ((pc (compiled-code-location-pc basic-code-location))
	 (debug-function (code-location-debug-function
			  basic-code-location))
	 (blocks (debug-function-debug-blocks debug-function))
	 (len (length blocks)))
    (declare (simple-vector blocks))
    (setf (code-location-%debug-block basic-code-location)
	  (if (= len 1)
	      (svref blocks 0)
	      (do ((i 1 (1+ i))
		   (end (1- len)))
		  ((= i end)
		   (let ((last (svref blocks end)))
		     (cond
		      ((debug-block-elsewhere-p last)
		       (if (< pc
			      (c::compiled-debug-function-elsewhere-pc
			       (compiled-debug-function-compiler-debug-fun
				debug-function)))
			   (svref blocks (1- end))
			   last))
		      ((< pc
			  (compiled-code-location-pc
			   (svref (compiled-debug-block-code-locations last)
				  0)))
		       (svref blocks (1- end)))
		      (t last))))
		(declare (type c::index i end))
		(when (< pc
			 (compiled-code-location-pc
			  (svref (compiled-debug-block-code-locations
				  (svref blocks i))
				 0)))
		  (return (svref blocks (1- i)))))))))

;;; CODE-LOCATION-DEBUG-SOURCE -- Public.
;;;
(defun code-location-debug-source (code-location)
  "Return $code-location's debug-source."
  (etypecase code-location
    (compiled-code-location
     (let* ((info (compiled-debug-function-debug-info
		   (code-location-debug-function code-location)))
	    (sources (c::compiled-debug-info-source info))
	    (len (length sources)))
       (declare (list sources))
       (when (zerop len)
	 (debug-signal 'no-debug-blocks :debug-function
		       (code-location-debug-function code-location)))
       (if (= len 1)
	   (car sources)
	   (do ((prev sources src)
		(src (cdr sources) (cdr src))
		(offset (code-location-top-level-form-offset code-location)))
	       ((null src) (car prev))
	     (when (< offset (c::debug-source-source-root (car src)))
	       (return (car prev)))))))
    (interpreted-code-location
     (first
      (let ((c::*lexical-environment* (c::make-null-environment)))
	(c::debug-source-for-info
	 (c::component-source-info
	  (c::block-component
	   (c::node-block
	    (interpreted-code-location-ir1-node code-location))))))))))

;;; CODE-LOCATION-TOP-LEVEL-FORM-OFFSET -- Public.
;;;
(defun code-location-top-level-form-offset (code-location)
  "Return the number of top-level forms before the one containing
   $code-location as seen by the compiler in some compilation unit.

   A compilation unit may be more than a single file, as describe in the
   section in [FIX] [debug-sources]."
  (if (code-location-unknown-p code-location)
      (error 'unknown-code-location :code-location code-location))
  (let ((tlf-offset (code-location-%tlf-offset code-location)))
    (cond ((eq tlf-offset :unparsed)
	   (etypecase code-location
	     (compiled-code-location
	      (or (fill-in-code-location code-location)
		  ;; This check should be unnecessary.  We're missing debug
		  ;; info the compiler should have dumped.
		  (error "Unknown code location?  It should be known."))
	      (code-location-%tlf-offset code-location))
	     (interpreted-code-location
	      (setf (code-location-%tlf-offset code-location)
		    (c::source-path-tlf-number
		     (c::node-source-path
		      (interpreted-code-location-ir1-node code-location)))))))
	  (t tlf-offset))))

;;; CODE-LOCATION-FORM-NUMBER -- Public.
;;;
(defun code-location-form-number (code-location)
  "Return the number of the form corresponding to $code-location.

   The form number is derived by walking the subforms of a top-level form
   in depth-first order.  While walking the top-level form, count one in
   depth-first order for each subform that is a cons.  Related to
   `form-number-translations'."
  (if (code-location-unknown-p code-location)
      (error 'unknown-code-location :code-location code-location))
  (let ((form-num (code-location-%form-number code-location)))
    (cond ((eq form-num :unparsed)
	   (etypecase code-location
	     (compiled-code-location
	      (or (fill-in-code-location code-location)
		  ;; This check should be unnecessary.  We're missing debug
		  ;; info the compiler should have dumped.
		  (error "Unknown code location?  It should be known."))
	      (code-location-%form-number code-location))
	     (interpreted-code-location
	      (setf (code-location-%form-number code-location)
		    (c::source-path-form-number
		     (c::node-source-path
		      (interpreted-code-location-ir1-node code-location)))))))
	  (t form-num))))

;;; CODE-LOCATION-KIND -- Public
;;;
(defun code-location-kind (code-location)
  "Return the kind of $code-location, one of:
     :interpreted, :unknown-return, :known-return, :internal-error,
     :non-local-exit, :block-start, :call-site, :single-value-return,
     :non-local-entry"
  (if (code-location-unknown-p code-location)
      (error 'unknown-code-location :code-location code-location))
  (etypecase code-location
    (compiled-code-location
     (let ((kind (compiled-code-location-kind code-location)))
       (cond ((not (eq kind :unparsed)) kind)
             ((not (fill-in-code-location code-location))
              ;; This check should be unnecessary.  We're missing
              ;; debug info the compiler should have dumped.
              (error "Unknown code location?  It should be known."))
             (t
              (compiled-code-location-kind code-location)))))
    (interpreted-code-location
     :interpreted)))

;;; COMPILED-CODE-LOCATION-LIVE-SET -- Internal.
;;;
;;; This returns the code-location's live-set if it is available.  If there
;;; is no debug-block information, this returns nil.
;;;
(defun compiled-code-location-live-set (code-location)
  (if (code-location-unknown-p code-location)
      nil
      (let ((live-set (compiled-code-location-%live-set code-location)))
	(cond ((eq live-set :unparsed)
	       (unless (fill-in-code-location code-location)
		 ;; This check should be unnecessary.  We're missing debug info
		 ;; the compiler should have dumped.
		 (error "Unknown code location?  It should be known."))
	       (compiled-code-location-%live-set code-location))
	      (t live-set)))))

;;; CODE-LOCATION= -- Public.
;;;
(defun code-location= (obj1 obj2)
  "Return whether the code-locations $obj1 and $obj2 are the same place in
   the code."
  (etypecase obj1
    (compiled-code-location
     (etypecase obj2
       (compiled-code-location
	(and (eq (code-location-debug-function obj1)
		 (code-location-debug-function obj2))
	     (sub-compiled-code-location= obj1 obj2)))
       (interpreted-code-location
	nil)))
    (interpreted-code-location
     (etypecase obj2
       (compiled-code-location
	nil)
       (interpreted-code-location
	(eq (interpreted-code-location-ir1-node obj1)
	    (interpreted-code-location-ir1-node obj2)))))))
;;;
(defun sub-compiled-code-location= (obj1 obj2)
  (= (compiled-code-location-pc obj1)
     (compiled-code-location-pc obj2)))

;;; FILL-IN-CODE-LOCATION -- Internal.
;;;
;;; This fills in location's :unparsed slots.  It returns t or nil depending on
;;; whether the code-location was known in its debug-function's debug-block
;;; information.  This may signal a no-debug-blocks condition due to
;;; DEBUG-FUNCTION-DEBUG-BLOCKS, and it assumes the %unknown-p slot is already
;;; set or going to be set.
;;;
(defun fill-in-code-location (code-location)
  (declare (type compiled-code-location code-location))
  (let* ((debug-function (code-location-debug-function code-location))
	 (blocks (debug-function-debug-blocks debug-function)))
    (declare (simple-vector blocks))
    (dotimes (i (length blocks) nil)
      (let* ((block (svref blocks i))
	     (locations (compiled-debug-block-code-locations block)))
	(declare (simple-vector locations))
	(dotimes (j (length locations))
	  (let ((loc (svref locations j)))
	    (when (sub-compiled-code-location= code-location loc)
	      (setf (code-location-%debug-block code-location) block)
	      (setf (code-location-%tlf-offset code-location)
		    (code-location-%tlf-offset loc))
	      (setf (code-location-%form-number code-location)
		    (code-location-%form-number loc))
	      (setf (compiled-code-location-%live-set code-location)
		    (compiled-code-location-%live-set loc))
	      (setf (compiled-code-location-kind code-location)
		    (compiled-code-location-kind loc))
	      (return-from fill-in-code-location t))))))))


;;;; Debug-blocks.

#[ Debug-blocks

Debug-blocks contain information pertinent to a specific range of code in a
debug-function.

{function:di:do-debug-block-locations}
{function:di:debug-block-successors}
{function:di:debug-block-elsewhere-p}
]#

;;; DO-DEBUG-BLOCK-LOCATIONS -- Public.
;;;
(defmacro do-debug-block-locations ((code-var debug-block &optional return)
				    &body body)
  "Execute $body in a context with $code-var bound to each code-location in
   $debug-block.  Return the value of executing $result."
  (let ((code-locations (gensym))
	(i (gensym)))
    `(let ((,code-locations (debug-block-code-locations ,debug-block)))
       (declare (simple-vector ,code-locations))
       (dotimes (,i (length ,code-locations) ,return)
	 (let ((,code-var (svref ,code-locations ,i)))
	   ,@body)))))

;;; DEBUG-BLOCK-FUNCTION-NAME -- Internal.
;;;
(defun debug-block-function-name (debug-block)
  "Return the name of the function represented by $debug-function.  This
   may be a string, cons or symbol."
  (etypecase debug-block
    (compiled-debug-block
     (let ((code-locs (compiled-debug-block-code-locations debug-block)))
       (declare (simple-vector code-locs))
       (if (zerop (length code-locs))
	   "??? Failed to get name of $debug-block's function."
	   (debug-function-name
	    (code-location-debug-function (svref code-locs 0))))))
    (interpreted-debug-block
     (c::lambda-name (c::block-home-lambda
		      (interpreted-debug-block-ir1-block debug-block))))))

;;; DEBUG-BLOCK-CODE-LOCATIONS -- Internal.
;;;
(defun debug-block-code-locations (debug-block)
  (etypecase debug-block
    (compiled-debug-block
     (compiled-debug-block-code-locations debug-block))
    (interpreted-debug-block
     (interpreted-debug-block-code-locations debug-block))))

;;; INTERPRETED-DEBUG-BLOCK-CODE-LOCATIONS -- Internal.
;;;
(defun interpreted-debug-block-code-locations (debug-block)
  (let ((code-locs (interpreted-debug-block-locations debug-block)))
    (if (eq code-locs :unparsed)
	(with-parsing-buffer (buf)
	  (c::do-nodes (node cont (interpreted-debug-block-ir1-block
				   debug-block))
	    (vector-push-extend (make-interpreted-code-location
				 node
				 (make-interpreted-debug-function
				  (c::block-home-lambda (c::node-block node))))
				buf))
	  (setf (interpreted-debug-block-locations debug-block)
		(result buf)))
	code-locs)))


;;;; Variables.

#[ Debug-variables

Debug-variables represent the constant information about where the system
stores argument and local variable values.  The system uniquely identifies with
an integer every instance of a variable with a particular name and package.  To
access a value, you must supply the frame along with the debug-variable since
these are particular to a function, not every instance of a variable on the
stack.

{function:di:debug-variable-name}
{function:di:debug-variable-package}
{function:di:debug-variable-symbol}
{function:di:debug-variable-id}
{function:di:debug-variable-validity}
{function:di:debug-variable-value}
{function:di:debug-variable-valid-value}
]#

;;; DEBUG-VARIABLE-SYMBOL -- Public.
;;;
(defun debug-variable-symbol (debug-var)
  "Return the symbol from interning `debug-variable-name' of $debug-var in
   the package named by the `debug-variable-package' of $debug-var."
  (let ((package (debug-variable-package debug-var)))
    (if package
	(intern (debug-variable-name debug-var) package)
	(make-symbol (debug-variable-name debug-var)))))

;;; DEBUG-VARIABLE-VALID-VALUE -- Public.
;;;
(defun debug-variable-valid-value (debug-var frame)
  "Return the value stored for debug-variable in $frame if it is valid,
   else signal an invalid-value error."
  (or (eq (debug-variable-validity debug-var (frame-code-location frame))
	      :valid)
      (error 'invalid-value :debug-variable debug-var :frame frame))
  (debug-variable-value debug-var frame))

;;; DEBUG-VARIABLE-VALUE -- Public.
;;;
(defun debug-variable-value (debug-var frame)
  "Return the value stored for debug-variable in frame.

   This is `setf'able."
  (etypecase debug-var
    (compiled-debug-variable
     (check-type frame compiled-frame)
     (let ((res (access-compiled-debug-var-slot debug-var frame)))
       (if (indirect-value-cell-p res)
	   (c:value-cell-ref res)
	   res)))
    (interpreted-debug-variable
     (check-type frame interpreted-frame)
     (eval::leaf-value-lambda-var
      (interpreted-code-location-ir1-node (frame-code-location frame))
      (interpreted-debug-variable-ir1-var debug-var)
      (frame-pointer frame)
      (interpreted-frame-closure frame)))))

;;; ACCESS-COMPILED-DEBUG-VAR-SLOT -- Internal.
;;;
;;; This returns what is stored for the variable represented by debug-var
;;; relative to the frame.  This may be an indirect value cell if the
;;; variable is both closed over and set.
;;;
(defun access-compiled-debug-var-slot (debug-var frame)
  (let ((escaped (compiled-frame-escaped frame)))
    (if escaped
	(sub-access-debug-var-slot
	 (frame-pointer frame)
	 (compiled-debug-variable-sc-offset debug-var)
	 escaped)
	(sub-access-debug-var-slot
	 (frame-pointer frame)
	 (or (compiled-debug-variable-save-sc-offset debug-var)
	     (compiled-debug-variable-sc-offset debug-var))))))

;;; SUB-ACCESS-DEBUG-VAR-SLOT -- Internal.
;;;
#-x86
(defun sub-access-debug-var-slot (fp sc-offset &optional escaped)
  (macrolet ((with-escaped-value ((var) &body forms)
	       `(if escaped
		    (let ((,var (vm:sigcontext-register
				 escaped
				 (c:sc-offset-offset sc-offset))))
		      ,@forms)
		    :invalid-value-for-unescaped-register-storage))
	     (escaped-float-value (format)
	       `(if escaped
		    (vm:sigcontext-float-register
		     escaped
		     (c:sc-offset-offset sc-offset)
		     ',format)
		    :invalid-value-for-unescaped-register-storage))
	     (with-nfp ((var) &body body)
	       `(let ((,var (if escaped
				(system:int-sap
				 (vm:sigcontext-register escaped
							 vm::nfp-offset))
				#-alpha
				(system:sap-ref-sap fp (* vm::nfp-save-offset
							  vm:word-bytes))
				#+alpha
				(alpha::make-number-stack-pointer
				 (system:sap-ref-32 fp (* vm::nfp-save-offset
							  vm:word-bytes))))))
		  ,@body)))
    (ecase (c:sc-offset-scn sc-offset)
      ((#.vm:any-reg-sc-number
	#.vm:descriptor-reg-sc-number
	#+rt #.vm:word-pointer-reg-sc-number)
       (system:without-gcing
	(with-escaped-value (val)
	  (kernel:make-lisp-obj val))))
      (#.vm:base-char-reg-sc-number
       (with-escaped-value (val)
	 (code-char val)))
      (#.vm:sap-reg-sc-number
       (with-escaped-value (val)
	 (system:int-sap val)))
      (#.vm:signed-reg-sc-number
       (with-escaped-value (val)
	 (if (logbitp (1- vm:word-bits) val)
	     (logior val (ash -1 vm:word-bits))
	     val)))
      (#.vm:unsigned-reg-sc-number
       (with-escaped-value (val)
	 val))
      (#.vm:non-descriptor-reg-sc-number
       (error "Local non-descriptor register access?"))
      (#.vm:interior-reg-sc-number
       (error "Local interior register access?"))
      (#.vm:single-reg-sc-number
       (escaped-float-value single-float))
      (#.vm:double-reg-sc-number
       (escaped-float-value double-float))
      #+long-float
      (#.vm:long-reg-sc-number
       (escaped-float-value long-float))
      (#.vm:complex-single-reg-sc-number
       (if escaped
	   (complex
	    (vm:sigcontext-float-register
	     escaped (c:sc-offset-offset sc-offset) 'single-float)
	    (vm:sigcontext-float-register
	     escaped (1+ (c:sc-offset-offset sc-offset)) 'single-float))
	   :invalid-value-for-unescaped-register-storage))
      (#.vm:complex-double-reg-sc-number
       (if escaped
	   (complex
	    (vm:sigcontext-float-register
	     escaped (c:sc-offset-offset sc-offset) 'double-float)
	    (vm:sigcontext-float-register
	     escaped (+ (c:sc-offset-offset sc-offset) #+sparc 2 #-sparc 1)
	     'double-float))
	   :invalid-value-for-unescaped-register-storage))
      #+long-float
      (#.vm:complex-long-reg-sc-number
       (if escaped
	   (complex
	    (vm:sigcontext-float-register
	     escaped (c:sc-offset-offset sc-offset) 'long-float)
	    (vm:sigcontext-float-register
	     escaped (+ (c:sc-offset-offset sc-offset) #+sparc 4)
	     'long-float))
	   :invalid-value-for-unescaped-register-storage))
      (#.vm:single-stack-sc-number
       (with-nfp (nfp)
	 (system:sap-ref-single nfp (* (c:sc-offset-offset sc-offset)
				       vm:word-bytes))))
      (#.vm:double-stack-sc-number
       (with-nfp (nfp)
	 (system:sap-ref-double nfp (* (c:sc-offset-offset sc-offset)
				       vm:word-bytes))))
      #+long-float
      (#.vm:long-stack-sc-number
       (with-nfp (nfp)
	 (system:sap-ref-long nfp (* (c:sc-offset-offset sc-offset)
				     vm:word-bytes))))
      (#.vm:complex-single-stack-sc-number
       (with-nfp (nfp)
	 (complex
	  (system:sap-ref-single nfp (* (c:sc-offset-offset sc-offset)
					vm:word-bytes))
	  (system:sap-ref-single nfp (* (1+ (c:sc-offset-offset sc-offset))
					vm:word-bytes)))))
      (#.vm:complex-double-stack-sc-number
       (with-nfp (nfp)
	 (complex
	  (system:sap-ref-double nfp (* (c:sc-offset-offset sc-offset)
					vm:word-bytes))
	  (system:sap-ref-double nfp (* (+ (c:sc-offset-offset sc-offset) 2)
					vm:word-bytes)))))
      #+long-float
      (#.vm:complex-long-stack-sc-number
       (with-nfp (nfp)
	 (complex
	  (system:sap-ref-long nfp (* (c:sc-offset-offset sc-offset)
				      vm:word-bytes))
	  (system:sap-ref-long nfp (* (+ (c:sc-offset-offset sc-offset)
					 #+sparc 4)
				      vm:word-bytes)))))
      (#.vm:control-stack-sc-number
       (kernel:stack-ref fp (c:sc-offset-offset sc-offset)))
      (#.vm:base-char-stack-sc-number
       (with-nfp (nfp)
	 (code-char (system:sap-ref-32 nfp (* (c:sc-offset-offset sc-offset)
					      vm:word-bytes)))))
      (#.vm:unsigned-stack-sc-number
       (with-nfp (nfp)
	 (system:sap-ref-32 nfp (* (c:sc-offset-offset sc-offset)
				   vm:word-bytes))))
      (#.vm:signed-stack-sc-number
       (with-nfp (nfp)
	 (system:signed-sap-ref-32 nfp (* (c:sc-offset-offset sc-offset)
					  vm:word-bytes))))
      (#.vm:sap-stack-sc-number
       (with-nfp (nfp)
	 (system:sap-ref-sap nfp (* (c:sc-offset-offset sc-offset)
				    vm:word-bytes)))))))

#+x86
(defun sub-access-debug-var-slot (fp sc-offset &optional escaped)
  (declare (type system:system-area-pointer fp))
  (macrolet ((with-escaped-value ((var) &body forms)
	       `(if escaped
		 (let ((,var (vm:sigcontext-register
			      escaped (c:sc-offset-offset sc-offset))))
		   ,@forms)
		 :invalid-value-for-unescaped-register-storage))
	     (escaped-float-value (format)
	       `(if escaped
		 (vm:sigcontext-float-register
		  escaped (c:sc-offset-offset sc-offset) ',format)
		 :invalid-value-for-unescaped-register-storage))
	     (escaped-complex-float-value (format)
	       `(if escaped
		 (complex
		  (vm:sigcontext-float-register
		   escaped (c:sc-offset-offset sc-offset) ',format)
		  (vm:sigcontext-float-register
		   escaped (1+ (c:sc-offset-offset sc-offset)) ',format))
		 :invalid-value-for-unescaped-register-storage))
	     ;; The debug variable locations are not always valid, and
	     ;; on the x86 locations can contain raw values.  To
	     ;; prevent later problems from invalid objects, they are
	     ;; filtered here.
	     (make-valid-lisp-obj (val)
	       `(if (or
		     ;; Fixnum
		     (zerop (logand ,val 3))
		     ;; Character
		     (and (zerop (logand ,val #xffff0000)) ; Top bits zero
		      (= (logand ,val #xff) vm:base-char-type)) ; Char tag
		     ;; Unbound marker.
		     (= ,val vm:unbound-marker-type)
		     ;; Pointer
		     (and (logand ,val 1)
		      ;; Check that the pointer is valid. X Could do a
		      ;; better job.
		      (or (< (lisp::read-only-space-start) ,val
			     (* lisp::*read-only-space-free-pointer*
				vm:word-bytes))
			  (< (lisp::static-space-start) ,val
			     (* lisp::*static-space-free-pointer*
				vm:word-bytes))
			  (< (lisp::current-dynamic-space-start) ,val
			     (sap-int (kernel:dynamic-space-free-pointer))))))
		 (kernel:make-lisp-obj ,val)
		 :invalid-object)))
    (ecase (c:sc-offset-scn sc-offset)
      ((#.vm:any-reg-sc-number #.vm:descriptor-reg-sc-number)
       (system:without-gcing
	(with-escaped-value (val)
	  (make-valid-lisp-obj val))))
      (#.vm:base-char-reg-sc-number
       (with-escaped-value (val)
	 (code-char val)))
      (#.vm:sap-reg-sc-number
       (with-escaped-value (val)
	 (system:int-sap val)))
      (#.vm:signed-reg-sc-number
       (with-escaped-value (val)
	 (if (logbitp (1- vm:word-bits) val)
	     (logior val (ash -1 vm:word-bits))
	     val)))
      (#.vm:unsigned-reg-sc-number
       (with-escaped-value (val)
	 val))
      (#.vm:single-reg-sc-number
       (escaped-float-value single-float))
      (#.vm:double-reg-sc-number
       (escaped-float-value double-float))
      #+long-float
      (#.vm:long-reg-sc-number
       (escaped-float-value long-float))
      (#.vm:complex-single-reg-sc-number
       (escaped-complex-float-value single-float))
      (#.vm:complex-double-reg-sc-number
       (escaped-complex-float-value double-float))
      #+long-float
      (#.vm:complex-long-reg-sc-number
       (escaped-complex-float-value long-float))
      (#.vm:single-stack-sc-number
       (system:sap-ref-single fp (- (* (1+ (c:sc-offset-offset sc-offset))
				       vm:word-bytes))))
      (#.vm:double-stack-sc-number
       (system:sap-ref-double fp (- (* (+ (c:sc-offset-offset sc-offset) 2)
				       vm:word-bytes))))
      #+long-float
      (#.vm:long-stack-sc-number
       (system:sap-ref-long fp (- (* (+ (c:sc-offset-offset sc-offset) 3)
				     vm:word-bytes))))
      (#.vm:complex-single-stack-sc-number
       (complex
	(system:sap-ref-single fp (- (* (1+ (c:sc-offset-offset sc-offset))
					vm:word-bytes)))
	(system:sap-ref-single fp (- (* (+ (c:sc-offset-offset sc-offset) 2)
					vm:word-bytes)))))
      (#.vm:complex-double-stack-sc-number
       (complex
	(system:sap-ref-double fp (- (* (+ (c:sc-offset-offset sc-offset) 2)
					vm:word-bytes)))
	(system:sap-ref-double fp (- (* (+ (c:sc-offset-offset sc-offset) 4)
					vm:word-bytes)))))
      #+long-float
      (#.vm:complex-long-stack-sc-number
       (complex
	(system:sap-ref-long fp (- (* (+ (c:sc-offset-offset sc-offset) 3)
				      vm:word-bytes)))
	(system:sap-ref-long fp (- (* (+ (c:sc-offset-offset sc-offset) 6)
				      vm:word-bytes)))))
      (#.vm:control-stack-sc-number
       (kernel:stack-ref fp (c:sc-offset-offset sc-offset)))
      (#.vm:base-char-stack-sc-number
       (code-char
	(system:sap-ref-32 fp (- (* (1+ (c:sc-offset-offset sc-offset))
				    vm:word-bytes)))))
      (#.vm:unsigned-stack-sc-number
       (system:sap-ref-32 fp (- (* (1+ (c:sc-offset-offset sc-offset))
				   vm:word-bytes))))
      (#.vm:signed-stack-sc-number
       (system:signed-sap-ref-32 fp (- (* (1+ (c:sc-offset-offset sc-offset))
					  vm:word-bytes))))
      (#.vm:sap-stack-sc-number
       (system:sap-ref-sap fp (- (* (1+ (c:sc-offset-offset sc-offset))
				    vm:word-bytes)))))))

;;; %SET-DEBUG-VARIABLE-VALUE -- Internal.
;;;
;;; This stores value as the value of debug-var in frame.  In the
;;; compiled-debug-variable case, access the current value to determine if it
;;; is an indirect value cell.  This occurs when the variable is both closed
;;; over and set.  For interpreted-debug-variables just call
;;; EVAL::SET-LEAF-VALUE-LAMBDA-VAR with the right interpreter objects.
;;;
(defun %set-debug-variable-value (debug-var frame value)
  (etypecase debug-var
    (compiled-debug-variable
     (check-type frame compiled-frame)
     (let ((current-value (access-compiled-debug-var-slot debug-var frame)))
       (if (indirect-value-cell-p current-value)
	   (c:value-cell-set current-value value)
	   (set-compiled-debug-variable-slot debug-var frame value))))
    (interpreted-debug-variable
     (check-type frame interpreted-frame)
     (eval::set-leaf-value-lambda-var
      (interpreted-code-location-ir1-node (frame-code-location frame))
      (interpreted-debug-variable-ir1-var debug-var)
      (frame-pointer frame)
      (interpreted-frame-closure frame)
      value)))
  value)
;;;
(defsetf debug-variable-value %set-debug-variable-value)

;;; SET-COMPILED-DEBUG-VARIABLE-SLOT -- Internal.
;;;
;;; This stores value for the variable represented by debug-var relative to the
;;; frame.  This assumes the location directly contains the variable's value;
;;; that is, there is no indirect value cell currently there in case the
;;; variable is both closed over and set.
;;;
(defun set-compiled-debug-variable-slot (debug-var frame value)
  (let ((escaped (compiled-frame-escaped frame)))
    (if escaped
	(sub-set-debug-var-slot (frame-pointer frame)
				(compiled-debug-variable-sc-offset debug-var)
				value escaped)
	(sub-set-debug-var-slot
	 (frame-pointer frame)
	 (or (compiled-debug-variable-save-sc-offset debug-var)
	     (compiled-debug-variable-sc-offset debug-var))
	 value))))

;;; SUB-SET-DEBUG-VAR-SLOT -- Internal.
;;;
#-x86
(defun sub-set-debug-var-slot (fp sc-offset value &optional escaped)
  (macrolet ((set-escaped-value (val)
	       `(if escaped
		    (setf (vm:sigcontext-register
			   escaped
			   (c:sc-offset-offset sc-offset))
			  ,val)
		    value))
	     (set-escaped-float-value (format val)
	       `(if escaped
		    (setf (vm:sigcontext-float-register
			   escaped
			   (c:sc-offset-offset sc-offset)
			   ',format)
			  ,val)
		    value))
	     (with-nfp ((var) &body body)
	       `(let ((,var (if escaped
				(system:int-sap
				 (vm:sigcontext-register escaped
							 vm::nfp-offset))
				#-alpha
				(system:sap-ref-sap fp
						    (* vm::nfp-save-offset
						       vm:word-bytes))
				#+alpha
				(alpha::make-number-stack-pointer
				 (system:sap-ref-32 fp
						    (* vm::nfp-save-offset
						       vm:word-bytes))))))
		  ,@body)))
    (ecase (c:sc-offset-scn sc-offset)
      ((#.vm:any-reg-sc-number
	#.vm:descriptor-reg-sc-number
	#+rt #.vm:word-pointer-reg-sc-number)
       (system:without-gcing
	(set-escaped-value
	  (kernel:get-lisp-obj-address value))))
      (#.vm:base-char-reg-sc-number
       (set-escaped-value (char-code value)))
      (#.vm:sap-reg-sc-number
       (set-escaped-value (system:sap-int value)))
      (#.vm:signed-reg-sc-number
       (set-escaped-value (logand value (1- (ash 1 vm:word-bits)))))
      (#.vm:unsigned-reg-sc-number
       (set-escaped-value value))
      (#.vm:non-descriptor-reg-sc-number
       (error "Local non-descriptor register access?"))
      (#.vm:interior-reg-sc-number
       (error "Local interior register access?"))
      (#.vm:single-reg-sc-number
       (set-escaped-float-value single-float value))
      (#.vm:double-reg-sc-number
       (set-escaped-float-value double-float value))
      #+long-float
      (#.vm:long-reg-sc-number
       (set-escaped-float-value long-float value))
      (#.vm:complex-single-reg-sc-number
       (when escaped
	 (setf (vm:sigcontext-float-register
		escaped (c:sc-offset-offset sc-offset) 'single-float)
	       (realpart value))
	 (setf (vm:sigcontext-float-register
		escaped (1+ (c:sc-offset-offset sc-offset))
		'single-float)
	       (imagpart value)))
       value)
      (#.vm:complex-double-reg-sc-number
       (when escaped
	 (setf (vm:sigcontext-float-register
		escaped (c:sc-offset-offset sc-offset) 'double-float)
	       (realpart value))
	 (setf (vm:sigcontext-float-register
		escaped
		(+ (c:sc-offset-offset sc-offset) #+sparc 2 #-sparc 1)
		'double-float)
	       (imagpart value)))
       value)
      #+long-float
      (#.vm:complex-long-reg-sc-number
       (when escaped
	 (setf (vm:sigcontext-float-register
		escaped (c:sc-offset-offset sc-offset) 'long-float)
	       (realpart value))
	 (setf (vm:sigcontext-float-register
		escaped
		(+ (c:sc-offset-offset sc-offset) #+sparc 4)
		'long-float)
	       (imagpart value)))
       value)
      (#.vm:single-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:sap-ref-single nfp (* (c:sc-offset-offset sc-offset)
					     vm:word-bytes))
	       (the single-float value))))
      (#.vm:double-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:sap-ref-double nfp (* (c:sc-offset-offset sc-offset)
					     vm:word-bytes))
	       (the double-float value))))
      #+long-float
      (#.vm:long-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:sap-ref-long nfp (* (c:sc-offset-offset sc-offset)
					   vm:word-bytes))
	       (the long-float value))))
      (#.vm:complex-single-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:sap-ref-single
		nfp (* (c:sc-offset-offset sc-offset) vm:word-bytes))
	       (the single-float (realpart value)))
	 (setf (system:sap-ref-single
		nfp (* (1+ (c:sc-offset-offset sc-offset)) vm:word-bytes))
	       (the single-float (realpart value)))))
      (#.vm:complex-double-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:sap-ref-double
		nfp (* (c:sc-offset-offset sc-offset) vm:word-bytes))
	       (the double-float (realpart value)))
	 (setf (system:sap-ref-double
		nfp (* (+ (c:sc-offset-offset sc-offset) 2) vm:word-bytes))
	       (the double-float (realpart value)))))
      #+long-float
      (#.vm:complex-long-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:sap-ref-long
		nfp (* (c:sc-offset-offset sc-offset) vm:word-bytes))
	       (the long-float (realpart value)))
	 (setf (system:sap-ref-long
		nfp (* (+ (c:sc-offset-offset sc-offset) #+sparc 4)
		       vm:word-bytes))
	       (the long-float (realpart value)))))
      (#.vm:control-stack-sc-number
       (setf (kernel:stack-ref fp (c:sc-offset-offset sc-offset)) value))
      (#.vm:base-char-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:sap-ref-32 nfp (* (c:sc-offset-offset sc-offset)
					 vm:word-bytes))
	       (char-code (the character value)))))
      (#.vm:unsigned-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:sap-ref-32 nfp (* (c:sc-offset-offset sc-offset)
					 vm:word-bytes))
	       (the (unsigned-byte 32) value))))
      (#.vm:signed-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:signed-sap-ref-32 nfp (* (c:sc-offset-offset sc-offset)
						vm:word-bytes))
	       (the (signed-byte 32) value))))
      (#.vm:sap-stack-sc-number
       (with-nfp (nfp)
	 (setf (system:sap-ref-sap nfp (* (c:sc-offset-offset sc-offset)
					  vm:word-bytes))
	       (the system:system-area-pointer value)))))))

#+x86
(defun sub-set-debug-var-slot (fp sc-offset value &optional escaped)
  (macrolet ((set-escaped-value (val)
	       `(if escaped
		    (setf (vm:sigcontext-register
			   escaped
			   (c:sc-offset-offset sc-offset))
			  ,val)
		    value)))
    (ecase (c:sc-offset-scn sc-offset)
      ((#.vm:any-reg-sc-number #.vm:descriptor-reg-sc-number)
       (system:without-gcing
	(set-escaped-value
	  (kernel:get-lisp-obj-address value))))
      (#.vm:base-char-reg-sc-number
       (set-escaped-value (char-code value)))
      (#.vm:sap-reg-sc-number
       (set-escaped-value (system:sap-int value)))
      (#.vm:signed-reg-sc-number
       (set-escaped-value (logand value (1- (ash 1 vm:word-bits)))))
      (#.vm:unsigned-reg-sc-number
       (set-escaped-value value))
      (#.vm:single-reg-sc-number
	#+nil ;; don't have escaped floats.
       (set-escaped-float-value single-float value))
      (#.vm:double-reg-sc-number
	#+nil ;;  don't have escaped floats -- still in npx?
       (set-escaped-float-value double-float value))
      #+long-float
      (#.vm:long-reg-sc-number
	#+nil ;;  don't have escaped floats -- still in npx?
       (set-escaped-float-value long-float value))
      (#.vm:single-stack-sc-number
       (setf (system:sap-ref-single
	      fp (- (* (1+ (c:sc-offset-offset sc-offset)) vm:word-bytes)))
	     (the single-float value)))
      (#.vm:double-stack-sc-number
       (setf (system:sap-ref-double
	      fp (- (* (+ (c:sc-offset-offset sc-offset) 2) vm:word-bytes)))
	     (the double-float value)))
      #+long-float
      (#.vm:long-stack-sc-number
       (setf (system:sap-ref-long
	      fp (- (* (+ (c:sc-offset-offset sc-offset) 3) vm:word-bytes)))
	     (the long-float value)))
      (#.vm:complex-single-stack-sc-number
       (setf (system:sap-ref-single
	      fp (- (* (1+ (c:sc-offset-offset sc-offset)) vm:word-bytes)))
	     (realpart (the (complex single-float) value)))
       (setf (system:sap-ref-single
	      fp (- (* (+ (c:sc-offset-offset sc-offset) 2) vm:word-bytes)))
	     (imagpart (the (complex single-float) value))))
      (#.vm:complex-double-stack-sc-number
       (setf (system:sap-ref-double
	      fp (- (* (+ (c:sc-offset-offset sc-offset) 2) vm:word-bytes)))
	     (realpart (the (complex double-float) value)))
       (setf (system:sap-ref-double
	      fp (- (* (+ (c:sc-offset-offset sc-offset) 4) vm:word-bytes)))
	     (imagpart (the (complex double-float) value))))
      #+long-float
      (#.vm:complex-long-stack-sc-number
       (setf (system:sap-ref-long
	      fp (- (* (+ (c:sc-offset-offset sc-offset) 3) vm:word-bytes)))
	     (realpart (the (complex long-float) value)))
       (setf (system:sap-ref-long
	      fp (- (* (+ (c:sc-offset-offset sc-offset) 6) vm:word-bytes)))
	     (imagpart (the (complex long-float) value))))
      (#.vm:control-stack-sc-number
       (setf (kernel:stack-ref fp (c:sc-offset-offset sc-offset)) value))
      (#.vm:base-char-stack-sc-number
       (setf (system:sap-ref-32 fp (- (* (1+ (c:sc-offset-offset sc-offset))
					 vm:word-bytes)))
	     (char-code (the character value))))
      (#.vm:unsigned-stack-sc-number
       (setf (system:sap-ref-32 fp (- (* (1+ (c:sc-offset-offset sc-offset))
					 vm:word-bytes)))
	     (the (unsigned-byte 32) value)))
      (#.vm:signed-stack-sc-number
       (setf (system:signed-sap-ref-32
	      fp (- (* (1+ (c:sc-offset-offset sc-offset)) vm:word-bytes)))
	     (the (signed-byte 32) value)))
      (#.vm:sap-stack-sc-number
       (setf (system:sap-ref-sap fp (- (* (1+ (c:sc-offset-offset sc-offset))
					  vm:word-bytes)))
	     (the system:system-area-pointer value))))))

(defsetf debug-variable-value %set-debug-variable-value)

;;; INDIRECT-VALUE-CELL-P -- Internal.
;;;
;;; The method for setting and accessing compiled-debug-variable values use
;;; this to determine if the value stored is the actual value or an indirection
;;; cell.
;;;
(defun indirect-value-cell-p (x)
  (and (= (kernel:get-lowtag x) vm:other-pointer-type)
       (= (kernel:get-type x) vm:value-cell-header-type)))

;;; DEBUG-VARIABLE-VALIDITY -- Public.
;;;
;;; If the variable is always alive, then it is valid.  If the code-location is
;;; unknown, then the variable's validity is :unknown.  Once we've called
;;; CODE-LOCATION-UNKNOWN-P, we know the live-set information has been cached
;;; in the code-location.
;;;
(defun debug-variable-validity (debug-var basic-code-loc)
  "Return three values reflecting the validity of $debug-var's value at
   $basic-code-location:
      :valid    The value is known to be available.
      :invalid  The value is known to be unavailable.
      :unknown  The value's availability is unknown."
  (etypecase debug-var
    (compiled-debug-variable
     (compiled-debug-variable-validity debug-var basic-code-loc))
    (interpreted-debug-variable
     (check-type basic-code-loc interpreted-code-location)
     (let ((validp (rassoc (interpreted-debug-variable-ir1-var debug-var)
			   (c::lexenv-variables
			    (c::node-lexenv
			     (interpreted-code-location-ir1-node
			      basic-code-loc))))))
       (if validp :valid :invalid)))))

;;; COMPILED-DEBUG-VARIABLE-VALIDITY -- Internal.
;;;
;;; This is the method for DEBUG-VARIABLE-VALIDITY for
;;; compiled-debug-variables.  For safety, make sure basic-code-loc is what
;;; we think.
;;;
(defun compiled-debug-variable-validity (debug-var basic-code-loc)
  (check-type basic-code-loc compiled-code-location)
  (cond ((debug-variable-alive-p debug-var)
	 (let ((debug-fun (code-location-debug-function basic-code-loc)))
	   (if (>= (compiled-code-location-pc basic-code-loc)
		   (c::compiled-debug-function-start-pc
		    (compiled-debug-function-compiler-debug-fun debug-fun)))
	       :valid
	       :invalid)))
	((code-location-unknown-p basic-code-loc) :unknown)
	(t
	 (let ((pos (position debug-var
			      (debug-function-debug-variables
			       (code-location-debug-function basic-code-loc)))))
	   (unless pos
	     (error 'unknown-debug-variable
		    :debug-variable debug-var
		    :debug-function
		    (code-location-debug-function basic-code-loc)))
	   ;; There must be live-set info since basic-code-loc is known.
	   (if (zerop (sbit (compiled-code-location-live-set basic-code-loc)
			    pos))
	       :invalid
	       :valid)))))


;;;; Sources.

#[ Source Translation Utilities

These two functions provide a mechanism for converting the rather obscure
(but highly compact) representation of source locations into an actual
source form:

{function:di:debug-source-start-positions}
{function:di:form-number-translations}
{function:di:source-path-context}
]#

;;; This code produces and uses what we call source-paths.  A source-path is a
;;; list whose first element is a form number as returned by
;;; CODE-LOCATION-FORM-NUMBER and whose last element is a top-level-form number
;;; as returned by CODE-LOCATION-TOP-LEVEL-FORM-NUMBER.  The elements from the
;;; last to the first, exclusively, are the numbered subforms into which to
;;; descend.  For example:
;;;    (defun foo (x)
;;;      (let ((a (aref x 3)))
;;;        (cons a 3)))
;;; The call to AREF in this example is form number 5.  Assuming this DEFUN is
;;; the 11'th top-level-form, the source-path for the AREF call is as follows:
;;;    (5 1 0 1 3 11)
;;; Given the DEFUN, 3 gets you the LET, 1 gets you the bindings, 0 gets the
;;; first binding, and 1 gets the AREF form.

;;; Temporary buffer used to build form-number => source-path translation in
;;; FORM-NUMBER-TRANSLATIONS.
;;;
(defvar *form-number-temp* (make-array 10 :fill-pointer 0 :adjustable t))

;;; Table used to detect CAR circularities in FORM-NUMBER-TRANSLATIONS.
;;;
(defvar *form-number-circularity-table* (make-hash-table :test #'eq))

;;; FORM-NUMBER-TRANSLATIONS  --  Public.
;;;
;;; The vector elements are in the same format as the compiler's
;;; NODE-SOUCE-PATH; that is, the first element is the form number and the
;;; last is the top-level-form number.
;;;
(defun form-number-translations (form tlf-number)
  "Return a table mapping form numbers to source-paths.  A source-path
   indicates a descent into the top-level-form form, going directly to the
   subform corressponding to the form number.  $tlf-number is the
   top-level-form number of $form."
  (clrhash *form-number-circularity-table*)
  (setf (fill-pointer *form-number-temp*) 0)
  (sub-translate-form-numbers form (list tlf-number))
  (coerce *form-number-temp* 'simple-vector))
;;;
(defun sub-translate-form-numbers (form path)
  (unless (gethash form *form-number-circularity-table*)
    (setf (gethash form *form-number-circularity-table*) t)
    (vector-push-extend (cons (fill-pointer *form-number-temp*) path)
			*form-number-temp*)
    (let ((pos 0)
	  (subform form)
	  (trail form))
      (declare (fixnum pos))
      (macrolet ((frob ()
		   '(progn
		      (when (atom subform) (return))
		      (let ((fm (car subform)))
			(when (consp fm)
			  (sub-translate-form-numbers fm (cons pos path)))
			(incf pos))
		      (setq subform (cdr subform))
		      (when (eq subform trail) (return)))))
	(loop
	  (frob)
	  (frob)
	  (setq trail (cdr trail)))))))

;;; SOURCE-PATH-CONTEXT  --  Public.
;;;
(defun source-path-context (form path context)
  "$form is a top-level form, and $path is a source-path into it.  Return
   the form indicated by the source-path.  $context is the number of
   enclosing forms to return instead of directly returning the source-path
   form.  When $context is greater than zero, the form returned contains a
   marker, #:****HERE****, immediately before the form indicated by path."
  (declare (type unsigned-byte context))
  ;;
  ;; Get to the form indicated by path or the enclosing form indicated by
  ;; context and path.
  (let ((path (reverse (butlast (cdr path)))))
    (dotimes (i (- (length path) context))
      (let ((index (first path)))
	(or (and (listp form) (< index (length form)))
	    (error "Source path no longer exists."))
	(setq form (elt form index))
	(setq path (rest path))))
    ;;
    ;; Recursively rebuild the source form resulting from the above descent,
    ;; copying the beginning of each subform up to the next subform we descend
    ;; into according to path.  At the bottom of the recursion, we return the
    ;; form indicated by path preceded by our marker, and this gets spliced
    ;; into the resulting list structure on the way back up.
    (labels ((frob (form path level)
	       (if (or (zerop level) (null path))
		   (if (zerop context)
		       form
		       `(#:***here*** ,form))
		   (let ((n (first path)))
		     (or (and (listp form) (< n (length form)))
			 (error "Source path no longer exists."))
		     (let ((res (frob (elt form n) (rest path) (1- level))))
		       (nconc (subseq form 0 n)
			      (cons res (nthcdr (1+ n) form))))))))
      (frob form path context))))


;;;; PREPROCESS-FOR-EVAL and EVAL-IN-FRAME.

;;; PREPROCESS-FOR-EVAL  --  Public.
;;;
;;; Create a SYMBOL-MACROLET for each variable valid at the location which
;;; accesses that variable from the frame argument.
;;;
(defun preprocess-for-eval (form loc)
  "Return a function of one argument that evaluates $form in the lexical
   context of the basic-code-location $loc.

   This allows efficient repeated evaluation of $form at a certain place in
   a function which could be useful for conditional breaking.

   $loc's debug-function must have debug-variable information available,
   else signal a no-debug-variables condition.

   The returned function takes the frame to get values from as its
   argument, and returns the values of $FORM.  The returned function
   signals the following conditions: invalid-value,
   ambiguous-variable-name, and frame-function-mismatch.

   Related to `eval-in-frame'."
  (declare (type code-location loc))
  (let ((n-frame (gensym))
	(fun (code-location-debug-function loc)))
    (or (debug-variable-info-available fun)
	(debug-signal 'no-debug-variables :debug-function fun))
    (ext:collect ((binds)
		  (specs))
      (do-debug-function-variables (var fun)
	(let ((validity (debug-variable-validity var loc)))
	  (or (eq validity :invalid)
	      (let* ((sym (debug-variable-symbol var))
		     (found (assoc sym (binds))))
		(if found
		    (setf (second found) :ambiguous)
		    (binds (list sym validity var)))))))
      (dolist (bind (binds))
	(let ((name (first bind))
	      (var (third bind)))
	  (ecase (second bind)
	    (:valid
	     (specs `(,name (debug-variable-value ',var ,n-frame))))
	    (:unknown
	     (specs `(,name (debug-signal 'invalid-value :debug-variable ',var
					  :frame ,n-frame))))
	    (:ambiguous
	     (specs `(,name (debug-signal 'ambiguous-variable-name :name ',name
					  :frame ,n-frame)))))))
      (let ((res (coerce `(lambda (,n-frame)
			    (declare (ignorable ,n-frame))
			    (symbol-macrolet ,(specs) ,form))
			 'function)))
	#'(lambda (frame)
	    ;; This prevents these functions from use in any location other
	    ;; than a function return location, so maybe this should only
	    ;; check whether frame's debug-function is the same as loc's.
	    (or (code-location= (frame-code-location frame) loc)
		(debug-signal 'frame-function-mismatch
			      :code-location loc :form form :frame frame))
	    (funcall res frame))))))

;;; EVAL-IN-FRAME  --  Public.
;;;
(defun eval-in-frame (frame form)
  (declare (type frame frame))
  "Evaluate $form in the lexical context of $frame's current code location,
   returning the results of the evaluation.

   Signal several debug-conditions since success relies on a variety of
   approximate debug information: invalid-value, ambiguous-variable-name,
   frame-function-mismatch.  Related to `preprocess-for-eval'."
  (funcall (preprocess-for-eval form (frame-code-location frame)) frame))


;;;; Breakpoints.

#[ Breakpoints

A breakpoint represents a function the system calls with the current frame when
execution passes a certain code-location.  A break point is active or inactive
independent of its existence.  They also have an extra slot for users to tag
the breakpoint with information.

{function:di:make-breakpoint}
{function:di:activate-breakpoint}
{function:di:deactivate-breakpoint}
{function:di:breakpoint-active-p}
{function:di:breakpoint-hook-function}
{function:di:breakpoint-info}
{function:di:breakpoint-kind}
{function:di:breakpoint-what}
{function:di:delete-breakpoint}
]#

;;;
;;; User visible interface.
;;;

(defun make-breakpoint (hook-function what
			&key (kind :code-location) info function-end-cookie)
  "Create and return a breakpoint.  When program execution encounters the
   breakpoint, the system calls $hook-function.  $hook-function takes the
   current frame for the function in which the program is running and the
   breakpoint object.

   $what and $kind determine where in a function the system invokes
   $hook-function.  $what is either a code-location or a debug-function.
   $kind is one of :code-location, :function-start, or :function-end.
   Since the starts and ends of functions may not have code-locations
   representing them, designate these places by supplying $what as a
   debug-function and $kind indicating the :function-start or
   :function-end.  When $what is a debug-function and $kind is
   :function-end, then $hook-function must take two additional arguments, a
   list of values returned by the function and a function-end-cookie.

   $info is information supplied by and used by the user.

   $function-end-cookie is a function.  To implement :function-end
   breakpoints, the system uses starter breakpoints to establish the
   :function-end breakpoint for each invocation of the function.  Upon each
   entry, the system creates a unique cookie to identify the invocation,
   and when the user supplies a function for this argument, the system
   invokes it on the frame and the cookie.  The system later invokes the
   :function-end breakpoint hook on the same cookie.  The user may save the
   cookie for comparison in the hook function.

   Signal an error if $what is an unknown code-location."
  (etypecase what
    (code-location
     (if (code-location-unknown-p what)
	 (error "Cannot make a breakpoint at an unknown code location -- ~S."
		what))
     (assert (eq kind :code-location))
     (let ((bpt (%make-breakpoint hook-function what kind info)))
       (etypecase what
	 (interpreted-code-location
	  (error "Breakpoints in interpreted code are currently unsupported."))
	 (compiled-code-location
	  ;; This slot is filled in due to calling CODE-LOCATION-UNKNOWN-P.
	  (if (eq (compiled-code-location-kind what) :unknown-return)
	      (let ((other-bpt (%make-breakpoint hook-function what
						 :unknown-return-partner
						 info)))
		(setf (breakpoint-unknown-return-partner bpt) other-bpt)
		(setf (breakpoint-unknown-return-partner other-bpt) bpt)))))
       bpt))
    (compiled-debug-function
     (ecase kind
       (:function-start
	(%make-breakpoint hook-function what kind info))
       (:function-end
	(or (eq (c::compiled-debug-function-returns
		 (compiled-debug-function-compiler-debug-fun what))
		:standard)
	    (error ":FUNCTION-END breakpoints are currently unsupported ~
		    for the known return convention."))

	(let* ((bpt (%make-breakpoint hook-function what kind info))
	       (starter (compiled-debug-function-end-starter what)))
	  (unless starter
	    (setf starter (%make-breakpoint #'list what :function-start nil))
	    (setf (breakpoint-hook-function starter)
		  (function-end-starter-hook starter what))
	    (setf (compiled-debug-function-end-starter what) starter))
	  (setf (breakpoint-start-helper bpt) starter)
	  (push bpt (breakpoint-%info starter))
	  (setf (breakpoint-cookie-fun bpt) function-end-cookie)
	  bpt))))
    (interpreted-debug-function
     (error ":function-end breakpoints are currently unsupported ~
	     for interpreted-debug-functions."))))

;;; These are unique objects created upon entry into a function by a
;;; :function-end breakpoint's starter hook.  These are only created when users
;;; supply :function-end-cookie to MAKE-BREAKPOINT.  Also, the :function-end
;;; breakpoint's hook is called on the same cookie when it is created.
;;;
(defstruct (function-end-cookie
	    (:print-function (lambda (obj str n)
			       (declare (ignore obj n))
			       (write-string "#<Function-End-Cookie>" str)))
	    (:constructor make-function-end-cookie (bogus-lra debug-fun)))
  ;; This is a pointer to the bogus-lra created for :function-end bpts.
  bogus-lra
  ;; This is the debug-function associated with the cookie.
  debug-fun)

;;; This maps bogus-lra-components to cookies, so
;;; HANDLE-FUNCTION-END-BREAKPOINT can find the appropriate cookie for the
;;; breakpoint hook.
;;;
(defvar *function-end-cookies* (make-hash-table :test #'eq))

;;; FUNCTION-END-STARTER-HOOK -- Internal.
;;;
;;; This returns a hook function for the start helper breakpoint associated
;;; with a :function-end breakpoint.  The returned function makes a fake LRA
;;; that all returns go through, and this piece of fake code actually breaks.
;;; Upon return from the break, the code provides the returnee with any values.
;;; Since the returned function effectively activates fun-end-bpt on each entry
;;; to debug-fun's function, we must establish breakpoint-data about
;;; fun-end-bpt.
;;;
(defun function-end-starter-hook (starter-bpt debug-fun)
  (declare (type breakpoint starter-bpt)
	   (type compiled-debug-function debug-fun))
  #'(lambda (frame breakpoint)
      (declare (ignore breakpoint)
	       (type frame frame))
      (let ((lra-sc-offset
	     (c::compiled-debug-function-return-pc
	      (compiled-debug-function-compiler-debug-fun debug-fun))))
	(multiple-value-bind (lra component offset)
			     (make-bogus-lra
			      (get-context-value frame
						 #-gengc vm::lra-save-offset
						 #+gengc vm::ra-save-offset
						 lra-sc-offset))
	  (setf (get-context-value frame
				   #-gengc vm::lra-save-offset
				   #+gengc vm::ra-save-offset
				   lra-sc-offset)
		lra)
	  (let ((end-bpts (breakpoint-%info starter-bpt)))
	    (let ((data (breakpoint-data component offset)))
	      (setf (breakpoint-data-breakpoints data) end-bpts)
	      (dolist (bpt end-bpts)
		(setf (breakpoint-internal-data bpt) data)))
	    (let ((cookie (make-function-end-cookie lra debug-fun)))
	      (setf (gethash component *function-end-cookies*) cookie)
	      (dolist (bpt end-bpts)
		(let ((fun (breakpoint-cookie-fun bpt)))
		  (when fun (funcall fun frame cookie))))))))))

;;; FUNCTION-END-COOKIE-VALID-P -- Public.
;;;
(defun function-end-cookie-valid-p (frame cookie)
  "Take a function-end-cookie $cookie and a $frame, and return whether the
   cookie is still valid.

   A cookie is valid until the frame that established the cookie exits.
   Sometimes cookie holders are unaware of cookie invalidation because
   their :function-end breakpoint hooks didn't run due to `throw'ing.  Take
   a frame as an efficiency hack since the user probably has a frame object
   in hand when using this routine, and it saves repeated parsing of the
   stack and consing when asking whether a series of cookies is valid."
  (let ((lra (function-end-cookie-bogus-lra cookie))
	(lra-sc-offset (c::compiled-debug-function-return-pc
			(compiled-debug-function-compiler-debug-fun
			 (function-end-cookie-debug-fun cookie)))))
    (do ((frame frame (frame-down frame)))
	((not frame) nil)
      (when (and (compiled-frame-p frame)
		 (#-x86 eq #+x86 sys:sap= lra
		     (get-context-value frame
					#-gengc vm::lra-save-offset
					#+gengc vm::ra-save-offset
					lra-sc-offset)))
	(return t)))))

;;;
;;; ACTIVATE-BREAKPOINT.
;;;

;;; ACTIVATE-BREAKPOINT -- Public.
;;;
(defun activate-breakpoint (breakpoint)
  "Cause the system to invoke the BREAKPOINT's hook-function until the next
   call to `deactivate-breakpoint' or `delete-breakpoint'.  The system
   invokes breakpoint hook functions in the opposite order that it
   activates them."
  (if (eq (breakpoint-status breakpoint) :deleted)
      (error "Cannot activate a deleted breakpoint -- ~S." breakpoint))
  (or (eq (breakpoint-status breakpoint) :active)
      (ecase (breakpoint-kind breakpoint)
	(:code-location
	 (let ((loc (breakpoint-what breakpoint)))
	   (etypecase loc
	     (interpreted-code-location
	      (error "Breakpoints in interpreted code are currently unsupported."))
	     (compiled-code-location
	      (activate-compiled-code-location-breakpoint breakpoint)
	      (let ((other (breakpoint-unknown-return-partner breakpoint)))
		(if other
		    (activate-compiled-code-location-breakpoint other)))))))
	(:function-start
	 (etypecase (breakpoint-what breakpoint)
	   (compiled-debug-function
	    (activate-compiled-function-start-breakpoint breakpoint))
	   (interpreted-debug-function
	    (error "I don't know how you made this, but they're unsupported -- ~S"
		   (breakpoint-what breakpoint)))))
	(:function-end
	 (etypecase (breakpoint-what breakpoint)
	   (compiled-debug-function
	    (let ((starter (breakpoint-start-helper breakpoint)))
	      (or (eq (breakpoint-status starter) :active)
		  ;; May already be active by some other :function-end breakpoint.
		  (activate-compiled-function-start-breakpoint starter)))
	    (setf (breakpoint-status breakpoint) :active))
	   (interpreted-debug-function
	    (error "I don't know how you made this, but they're unsupported -- ~S"
		   (breakpoint-what breakpoint)))))))
  breakpoint)

;;; ACTIVATE-COMPILED-CODE-LOCATION-BREAKPOINT -- Internal.
;;;
(defun activate-compiled-code-location-breakpoint (breakpoint)
  (declare (type breakpoint breakpoint))
  (let ((loc (breakpoint-what breakpoint)))
    (declare (type compiled-code-location loc))
    (sub-activate-breakpoint
     breakpoint
     (breakpoint-data (compiled-debug-function-component
		       (code-location-debug-function loc))
		      (+ (compiled-code-location-pc loc)
			 (if (or (eq (breakpoint-kind breakpoint)
				     :unknown-return-partner)
				 (eq (compiled-code-location-kind loc)
				     :single-value-return))
			     vm:single-value-return-byte-offset
			     0))))))

;;; ACTIVATE-COMPILED-FUNCTION-START-BREAKPOINT -- Internal.
;;;
(defun activate-compiled-function-start-breakpoint (breakpoint)
  (declare (type breakpoint breakpoint))
  (let ((debug-fun (breakpoint-what breakpoint)))
    (sub-activate-breakpoint
     breakpoint
     (breakpoint-data (compiled-debug-function-component debug-fun)
		      (c::compiled-debug-function-start-pc
		       (compiled-debug-function-compiler-debug-fun
			debug-fun))))))

;;; SUB-ACTIVATE-BREAKPOINT -- Internal.
;;;
(defun sub-activate-breakpoint (breakpoint data)
  (declare (type breakpoint breakpoint)
	   (type breakpoint-data data))
  (setf (breakpoint-status breakpoint) :active)
  (system:block-interrupts
   (unless (breakpoint-data-breakpoints data)
     (setf (breakpoint-data-instruction data)
	   (system:without-gcing
	    (breakpoint-install (kernel:get-lisp-obj-address
				 (breakpoint-data-component data))
				(breakpoint-data-offset data)))))
   (setf (breakpoint-data-breakpoints data)
	 (append (breakpoint-data-breakpoints data) (list breakpoint)))
   (setf (breakpoint-internal-data breakpoint) data)))

;;;
;;; DEACTIVATE-BREAKPOINT.
;;;

;;; DEACTIVATE-BREAKPOINT -- Public.
;;;
(defun deactivate-breakpoint (breakpoint)
  "Stop the system from invoking the $breakpoint's hook-function."
  (when (eq (breakpoint-status breakpoint) :active)
    (system:block-interrupts
     (let ((loc (breakpoint-what breakpoint)))
       (etypecase loc
	 ((or interpreted-code-location interpreted-debug-function)
	  (error
	   "Breakpoints in interpreted code are currently unsupported."))
	 ((or compiled-code-location compiled-debug-function)
	  (deactivate-compiled-breakpoint breakpoint)
	  (let ((other (breakpoint-unknown-return-partner breakpoint)))
	    (when other
	      (deactivate-compiled-breakpoint other))))))))
  breakpoint)

(defun deactivate-compiled-breakpoint (breakpoint)
  (if (eq (breakpoint-kind breakpoint) :function-end)
      (let ((starter (breakpoint-start-helper breakpoint)))
	(unless (find-if #'(lambda (bpt)
			     (and (not (eq bpt breakpoint))
				  (eq (breakpoint-status bpt) :active)))
			 (breakpoint-%info starter))
	  (deactivate-compiled-breakpoint starter)))
      (let* ((data (breakpoint-internal-data breakpoint))
	     (bpts (delete breakpoint (breakpoint-data-breakpoints data))))
	(setf (breakpoint-internal-data breakpoint) nil)
	(setf (breakpoint-data-breakpoints data) bpts)
	(unless bpts
	  (system:without-gcing
	   (breakpoint-remove (kernel:get-lisp-obj-address
			       (breakpoint-data-component data))
			      (breakpoint-data-offset data)
			      (breakpoint-data-instruction data)))
	  (delete-breakpoint-data data))))
  (setf (breakpoint-status breakpoint) :inactive)
  breakpoint)

;;;
;;; BREAKPOINT-INFO.
;;;

;;; BREAKPOINT-INFO -- Public.
;;;
(defun breakpoint-info (breakpoint)
  "Return the user maintained info associated with $breakpoint.

   This is `setf'able."
  (breakpoint-%info breakpoint))
;;;
(defun %set-breakpoint-info (breakpoint value)
  (setf (breakpoint-%info breakpoint) value)
  (let ((other (breakpoint-unknown-return-partner breakpoint)))
    (when other
      (setf (breakpoint-%info other) value))))
;;;
(defsetf breakpoint-info %set-breakpoint-info)

;;;
;;; BREAKPOINT-ACTIVE-P and DELETE-BREAKPOINT.
;;;

;;; BREAKPOINT-ACTIVE-P -- Public.
;;;
(defun breakpoint-active-p (breakpoint)
  "Return whether $breakpoint is currently active."
  (ecase (breakpoint-status breakpoint)
    (:active t)
    ((:inactive :deleted) nil)))

;;; DELETE-BREAKPOINT -- Public.
;;;
(defun delete-breakpoint (breakpoint)
  "Free system storage and remove computational overhead associated with
   breakpoint.  After calling this, breakpoint is completely impotent and
   can never become active again."
  (let ((status (breakpoint-status breakpoint)))
    (unless (eq status :deleted)
      (if (eq status :active)
	  (deactivate-breakpoint breakpoint))
      (setf (breakpoint-status breakpoint) :deleted)
      (let ((other (breakpoint-unknown-return-partner breakpoint)))
	(if other
	    (setf (breakpoint-status other) :deleted)))
      (if (eq (breakpoint-kind breakpoint) :function-end)
	  (let* ((starter (breakpoint-start-helper breakpoint))
	       (breakpoints (delete breakpoint
				    (the list (breakpoint-info starter)))))
	  (setf (breakpoint-info starter) breakpoints)
	  (unless breakpoints
	    (delete-breakpoint starter)
	    (setf (compiled-debug-function-end-starter
		   (breakpoint-what breakpoint))
		  nil))))))
  breakpoint)

;;;
;;; C call out stubs.
;;;

;;; BREAKPOINT-INSTALL -- Internal.
;;;
;;; This actually installs the break instruction in the component.  It returns
;;; the overwritten bits.  You must call this in a context in which GC is
;;; disabled, so Lisp doesn't move objects around that C is pointing to.
;;;
(alien:def-alien-routine "breakpoint_install" c-call:unsigned-long
  (code-obj c-call:unsigned-long)
  (pc-offset c-call:int))

;;; BREAKPOINT-REMOVE -- Internal.
;;;
;;; This removes the break instruction and replaces the original instruction.
;;; You must call this in a context in which GC is disabled, so Lisp doesn't
;;; move objects around that C is pointing to.
;;;
(alien:def-alien-routine "breakpoint_remove" c-call:void
  (code-obj c-call:unsigned-long)
  (pc-offset c-call:int)
  (old-inst c-call:unsigned-long))

(alien:def-alien-routine "breakpoint_do_displaced_inst" c-call:void
  (scp (* unix:sigcontext))
  (orig-inst c-call:unsigned-long))

;;;
;;; Breakpoint handlers (layer between C and exported interface).
;;;

;;; This maps components to a mapping of offsets to breakpoint-datas.
;;;
(defvar *component-breakpoint-offsets* (make-hash-table :test #'eq))

;;; BREAKPOINT-DATA -- Internal.
;;;
;;; This returns the breakpoint-data associated with component cross offset.
;;; If none exists, this makes one, installs it, and returns it.
;;;
(defun breakpoint-data (component offset &optional (create t))
  (flet ((install-breakpoint-data ()
	   (when create
	     (let ((data (make-breakpoint-data component offset)))
	       (push (cons offset data)
		     (gethash component *component-breakpoint-offsets*))
	       data))))
    (let ((offsets (gethash component *component-breakpoint-offsets*)))
      (if offsets
	  (let ((data (assoc offset offsets)))
	    (if data
		(cdr data)
		(install-breakpoint-data)))
	  (install-breakpoint-data)))))

;;; DELETE-BREAKPOINT-DATA -- Internal.
;;;
;;; We use this when there are no longer any active breakpoints corresponding
;;; to data.
;;;
(defun delete-breakpoint-data (data)
  (let* ((component (breakpoint-data-component data))
	 (offsets (delete (breakpoint-data-offset data)
			  (gethash component *component-breakpoint-offsets*)
			  :key #'car)))
    (if offsets
	(setf (gethash component *component-breakpoint-offsets*) offsets)
	(remhash component *component-breakpoint-offsets*)))
  (ext:undefined-value))

;;; HANDLE-BREAKPOINT -- Internal Interface.
;;;
;;; The C handler for interrupts calls this when it has a debugging-tool break
;;; instruction.  This does NOT handle all breaks; for example, it does not
;;; handle breaks for internal errors.
;;;
(defun handle-breakpoint (offset component signal-context)
  (let ((data (breakpoint-data component offset nil)))
    (unless data
      (error "Unknown breakpoint in ~S at offset ~S."
	      (debug-function-name (debug-function-from-pc component offset))
	      offset))
    (let ((breakpoints (breakpoint-data-breakpoints data)))
      (if (or (null breakpoints)
	      (eq (breakpoint-kind (car breakpoints)) :function-end))
	  (handle-function-end-breakpoint-aux breakpoints data signal-context)
	  (handle-breakpoint-aux breakpoints data
				 offset component signal-context)))))

;;; This holds breakpoint-datas while invoking the breakpoint hooks associated
;;; with that particular component and location.  While they are executing, if
;;; we hit the location again, we ignore the breakpoint to avoid infinite
;;; recursion.  Function-end breakpoints must work differently since the
;;; breakpoint-data is unique for each invocation.
;;;
(defvar *executing-breakpoint-hooks* nil)

;;; HANDLE-BREAKPOINT-AUX -- Internal.
;;;
;;; This handles code-location and debug-function :function-start breakpoints.
;;;
(defun handle-breakpoint-aux (breakpoints data offset component signal-context)
  (unless breakpoints
    (error "Breakpoint that nobody wants?"))
  (unless (member data *executing-breakpoint-hooks*)
    (let ((*executing-breakpoint-hooks* (cons data
					      *executing-breakpoint-hooks*)))
      (invoke-breakpoint-hooks breakpoints component offset)))
  ;; At this point breakpoints may not hold the same list as
  ;; BREAKPOINT-DATA-BREAKPOINTS since invoking hooks may have allowed a
  ;; breakpoint deactivation.  In fact, if all breakpoints were deactivated
  ;; then data is invalid since it was deleted and so the correct one must be
  ;; looked up if it is to be used.  If there are no more breakpoints active
  ;; at this location, then the normal instruction has been put back, and we
  ;; do not need to do-displaced-inst.
  (let ((data (breakpoint-data component offset nil)))
    (when (and data (breakpoint-data-breakpoints data))
      ;; There breakpoint is still active, so we need to execute the displaced
      ;; instruction and leave the breakpoint instruction behind.  The best
      ;; way to do this is different on each machine, so we just leave it up
      ;; to the C code.
      (breakpoint-do-displaced-inst signal-context
				    (breakpoint-data-instruction data))
      ; Under HPUX we can't sigreturn so bp-do-disp-i has to return.
      #-(or hpux irix x86)
      (error "BREAKPOINT-DO-DISPLACED-INST returned?"))))

(defun invoke-breakpoint-hooks (breakpoints component offset)
  (let* ((debug-fun (debug-function-from-pc component offset))
	 (frame (do ((f (top-frame) (frame-down f)))
		    ((eq debug-fun (frame-debug-function f)) f))))
    (dolist (bpt breakpoints)
      (funcall (breakpoint-hook-function bpt)
	       frame
	       ;; If this is an :unknown-return-partner, then pass the
	       ;; hook function the original breakpoint, so that users
	       ;; arn't forced to confront the fact that some breakpoints
	       ;; really are two.
	       (if (eq (breakpoint-kind bpt) :unknown-return-partner)
		   (breakpoint-unknown-return-partner bpt)
		   bpt)))))

;;; HANDLE-FUNCTION-END-BREAKPOINT -- Internal Interface
;;;
(defun handle-function-end-breakpoint (offset component sigcontext)
  (let ((data (breakpoint-data component offset nil)))
    (unless data
      (error "Unknown breakpoint in ~S at offset ~S."
	      (debug-function-name (debug-function-from-pc component offset))
	      offset))
    (let ((breakpoints (breakpoint-data-breakpoints data)))
      (when breakpoints
	(assert (eq (breakpoint-kind (car breakpoints)) :function-end))
	(handle-function-end-breakpoint-aux breakpoints data sigcontext)))))

;;; HANDLE-FUNCTION-END-BREAKPOINT-AUX -- Internal.
;;;
;;; Either HANDLE-BREAKPOINT calls this for :function-end breakpoints [old C
;;; code] or HANDLE-FUNCTION-END-BREAKPOINT calls this directly [new C code].
;;;
(defun handle-function-end-breakpoint-aux (breakpoints data signal-context)
  (delete-breakpoint-data data)
  (let* ((scp
	  (locally
	    (declare (optimize (ext:inhibit-warnings 3)))
	    (alien:sap-alien signal-context (* unix:sigcontext))))
	 (frame (do ((cfp (vm:sigcontext-register scp vm::cfp-offset))
		     (f (top-frame) (frame-down f)))
		    ((= cfp (system:sap-int (frame-pointer f))) f)
		  (declare (type (unsigned-byte #.vm:word-bits) cfp))))
	 (component (breakpoint-data-component data))
	 (cookie (gethash component *function-end-cookies*)))
    (remhash component *function-end-cookies*)
    (dolist (bpt breakpoints)
      (funcall (breakpoint-hook-function bpt)
	       frame bpt
	       (get-function-end-breakpoint-values scp)
	       cookie))))

(defun get-function-end-breakpoint-values (scp)
  (let ((ocfp (system:int-sap (vm:sigcontext-register scp
						      #-x86 vm::ocfp-offset
						      #+x86 vm::ebx-offset)))
	(nargs (kernel:make-lisp-obj
		(vm:sigcontext-register scp vm::nargs-offset)))
 	(reg-arg-offsets '#.vm::register-arg-offsets)
	(results nil))
    (system:without-gcing
     (dotimes (arg-num nargs)
       (push (if reg-arg-offsets
		 (kernel:make-lisp-obj
		  (vm:sigcontext-register scp (pop reg-arg-offsets)))
	       (kernel:stack-ref ocfp arg-num))
	     results)))
    (nreverse results)))

;;;
;;; MAKE-BOGUS-LRA (used for :function-end breakpoints)
;;;

(defconstant bogus-lra-constants #-x86 2 #+x86 3)
(defconstant known-return-p-slot (+ vm:code-constants-offset #-x86 1 #+x86 2))

;;; MAKE-BOGUS-LRA -- Interface.
;;;
(defun make-bogus-lra (real-lra &optional known-return-p)
  "Make a bogus LRA object that signals a breakpoint trap when returned to.
   If the breakpoint trap handler returns, $real-lra is returned to [FIX]
   too?.  Return three values: the bogus LRA object, the code component it
   is part of, and the PC offset for the trap instruction."
  (system:without-gcing
   (let* ((src-start (system:foreign-symbol-address
		      "function_end_breakpoint_guts"))
	  (src-end (system:foreign-symbol-address
		    "function_end_breakpoint_end"))
	  (trap-loc (system:foreign-symbol-address
		     "function_end_breakpoint_trap"))
	  (length (system:sap- src-end src-start))
	  (code-object
	   (system:%primitive
	    #-(and x86 gencgc) c:allocate-code-object
	    #+(and x86 gencgc) c::allocate-dynamic-code-object
	    (1+ bogus-lra-constants)
	    length))
	  (dst-start (kernel:code-instructions code-object)))
     (declare (type system:system-area-pointer
		    src-start src-end dst-start trap-loc)
	      (type kernel:index length))
     (setf (kernel:%code-debug-info code-object) :bogus-lra)
     (setf (kernel:code-header-ref code-object vm:code-trace-table-offset-slot)
	   length)
     #-x86
     (setf (kernel:code-header-ref code-object real-lra-slot) real-lra)
     #+x86
     (multiple-value-bind (offset code)
	 (compute-lra-data-from-pc real-lra)
       (setf (kernel:code-header-ref code-object real-lra-slot) code)
       (setf (kernel:code-header-ref code-object (1+ real-lra-slot)) offset))
     (setf (kernel:code-header-ref code-object known-return-p-slot)
	   known-return-p)
     (kernel:system-area-copy src-start 0 dst-start 0 (* length vm:byte-bits))
     (vm:sanctify-for-execution code-object)
     #+x86
     (values dst-start code-object (system:sap- trap-loc src-start))
     #-x86
     (let ((new-lra (kernel:make-lisp-obj (+ (system:sap-int dst-start)
					     vm:other-pointer-type))))
       (kernel:set-header-data
	new-lra
	(logandc2 (+ vm:code-constants-offset bogus-lra-constants 1)
		  1))
       (vm:sanctify-for-execution code-object)
       (values new-lra code-object (system:sap- trap-loc src-start))))))


;;;; Editor support.

;;; This holds breakpoints in the slave set on behalf of the editor.
;;;
; FIX?
;(defvar *editor-breakpoints* (make-hash-table :test #'equal))

;;;
;;; Setting breakpoints.
;;;

;;; SET-BREAKPOINT-FOR-EDITOR -- Internal Interface.
;;;
(defun set-breakpoint-for-editor (package name-str path)
  "The editor calls this remotely in the slave to set breakpoints.

   $package is the string name of a package or (), and $name-str is a
   string representing a function name (for example, \"foo\" or \"(setf
   foo)\").

   After finding package, `read' $name-str with *package* bound
   appropriately.

   $path is either a modified source-path or a symbol (:function-start or
   :function-end).  If it is a modified source-path, it has no
   top-level-form offset or form-number component, and it is in descent
   order from the root of the top-level form."
  (let* ((name (let ((*package* (if package
				    (lisp::package-or-lose package)
				    *package*)))
		 (read-from-string name-str)))
	 (debug-fun (function-debug-function (fdefinition name))))
    (etypecase path
      (symbol
       (let* ((bpt (di:make-breakpoint
		    #'(lambda (frame bpt)
			(declare (ignore frame bpt))
			(break "Editor installed breakpoint."))
		    debug-fun :kind path))
	      (remote-bpt (wire:make-remote-object bpt)))
	 (activate-breakpoint bpt)
	 ;;(push remote-bpt (gethash name *editor-breakpoints*))
	 remote-bpt))
      (cons
       (etypecase debug-fun
	 (compiled-debug-function
	  (compiled-debug-function-set-breakpoint-for-editor
	   debug-fun #|name|# path))
	 (interpreted-debug-function
	  (error
	   "We don't currently support breakpoints in interpreted code.")))))))

(defun compiled-debug-function-set-breakpoint-for-editor (debug-fun #|name|# path)
  (let* ((source-paths (generate-component-source-paths
			(compiled-debug-function-component debug-fun)))
	 (matches nil)
	 (matching-length 0))
    (declare (simple-vector source-paths)
	     (list matches)
	     (fixnum matching-length))
    ;; Build a list of paths that match path up to matching-length
    ;; elements.
    (macrolet ((maybe-store-match (path matched-len)
		 `(cond ((> ,matched-len matching-length)
			 (setf matches (list ,path))
			 (setf matching-length ,matched-len))
			((= ,matched-len matching-length)
			 (cons ,path matches)))))
      (dotimes (i (length source-paths))
	(declare (fixnum i))
	(let ((sp (svref source-paths i)))
	  ;; Remember, first element of sp is a code-location.
	  (do ((path-ptr path (cdr path-ptr))
	       (sp-ptr (cdr sp) (cdr sp-ptr))
	       (count 0 (1+ count)))
	      ((or (null path-ptr) (null sp-ptr))
	       (if (null sp-ptr)
		   (maybe-store-match sp count)))
	    (declare (list sp-ptr path-ptr)
		     (fixnum count))
	    (or (= (the fixnum (car path-ptr)) (the fixnum (car sp-ptr)))
		(maybe-store-match sp count))))))
    ;; If there's just one, set it; otherwise, return the conflict set.
    (cond ((and (= (length matches) 1) (equal path (cdar matches)))
	   (let* ((bpt (make-breakpoint
			#'(lambda (frame bpt)
			    (declare (ignore frame bpt))
			    (break "Editor installed breakpoint."))
			(wire:remote-object-value (caar matches))))
		  (remote-bpt (wire:make-remote-object bpt)))
	     (activate-breakpoint bpt)
	     ;;(push remote-bpt (gethash name *editor-breakpoints*))
	     remote-bpt))
	  (t matches))))

;;; This maps components to vectors of modified source-paths.  We assume users
;;; will set multiple breakpoints in a given function which entails computing
;;; this data repeatedly.  Possibly the GC hook should free this cache.  The
;;; source-paths are modified in the following ways:
;;;    1] The form number element (first) is clobbered with the code-location
;;;       corresponding to the source-path.
;;;    2] The top-level-form offset element (last) is thrown away.
;;;    3] Everything after the first element is reversed, so the modified
;;;       source-path actually portrays a descent into the form.
;;;
(defvar *component-source-locations* (make-hash-table :test #'eq))

;;; GENERATE-COMPONENT-SOURCE-PATHS -- Internal.
;;;
;;; This returns a vector of modified source-paths, one for every code-location
;;; in component.  The source-paths are modified as described for
;;; *component-source-locations*.
;;;
(defun generate-component-source-paths (component)
  (or (gethash component *component-source-locations*)
      (setf (gethash component *component-source-locations*)
	    (sub-generate-component-source-paths component))))

;;; This maps source-infos to hashtables that map top-level-form offsets to
;;; modified form-number translations (as returned by
;;; FORM-NUMBER-TRANSLATIONS).  These are modified as described for
;;; *component-source-locations*.
;;;
(defvar *source-info-offset-translations* (make-hash-table :test #'eq))

;;; This is a hacking space for SUB-GENERATE-COMPONENT-SOURCE-PATHS.  We use
;;; this because we throw away many source-paths we accumulate in this buffer
;;; since they are not associated with code-locations.
;;;
(defvar *source-paths-buffer* (make-array 50 :fill-pointer t :adjustable t))

;;; SUB-GENERATE-COMPONENT-SOURCE-PATHS -- Internal.
;;;
;;; We iterate over the code-locations in component, fetching their
;;; source-infos and using the *source-info-offset-translations* cache.  This
;;; computation often repeatedly sees the same source-info/tlf-offset pair, so
;;; we see many source-paths from one form-number-translation table.  Because
;;; of this, when we add a form-number-translations table to this cache, we add
;;; all the source-paths in it to the result immediately.  Then later if we see
;;; the same (not EQ though) source-info/tlf-offset form-number-translations,
;;; we can simply check if one of the source-paths is already in the result,
;;; and if it is, then all of them already are.  We keep the cache around
;;; between invocations since we expect multiple breakpoints to be set in the
;;; same function, and this is why we must check if a form-number-translations
;;; has been added to the result; just its presence in the cache does not mean
;;; it is in the result vector.
;;;
(defun sub-generate-component-source-paths (component)
  (let ((info (kernel:%code-debug-info component)))
    (or info (debug-signal 'no-debug-info))
    (let* ((function-map (get-debug-info-function-map info))
	   (result *source-paths-buffer*))
      (declare (simple-vector function-map)
	       (vector result))
      (setf (fill-pointer result) 0)
      (flet ((copy-stuff (form-num-trans result)
	       (declare (simple-vector form-num-trans)
			(vector result))
	       (dotimes (i (length form-num-trans))
		 (declare (fixnum i))
		 (vector-push-extend (svref form-num-trans i) result)))
	     (convert-paths (form-num-trans)
	       (declare (simple-vector form-num-trans))
	       (dotimes (i (length form-num-trans) form-num-trans)
		 (declare (fixnum i))
		 (let* ((source-path (svref form-num-trans i)))
		   (declare (list source-path))
		   ;; Make the first cons point to the reversal of everything
		   ;; else, but throw away what was the last element before the
		   ;; reversal.
		   (setf (cdr source-path)
			 ;; Must copy the rest of the list, so REVERSE, but
			 ;; the first cons cell of each list is unique.
			 (cdr (reverse (cdr source-path))))))))
	;; Get all possible source-paths, modifying any new additions to the
	;; cache.
	(do ((i 0 (+ i 2))
	     (len (length function-map)))
	    ((>= i len))
	  (declare (type c::index i))
	  (let ((d-fun (make-compiled-debug-function (svref function-map i)
						     component)))
	    (do-debug-function-blocks (d-block d-fun)
	      (do-debug-block-locations (loc d-block)
		(let* ((d-source (code-location-debug-source loc))
		       (translations (gethash d-source
					      *source-info-offset-translations*))
		       (tlf-offset (code-location-top-level-form-offset loc))
		       (loc-num (code-location-form-number loc)))
		  (cond
		   (translations
		    (let ((form-num-trans (gethash tlf-offset translations)))
		      (declare (type (or simple-vector null) form-num-trans))
		      (cond
		       ((not form-num-trans)
			(let ((form-num-trans (get-form-number-translations
					       d-source tlf-offset)))
			  (declare (simple-vector form-num-trans))
			  (setf (gethash tlf-offset translations) form-num-trans)
			  (copy-stuff (convert-paths form-num-trans) result)
			  (setf (car (svref form-num-trans loc-num))
				(wire:make-remote-object loc))))
		       ;; If one of these source-paths is in our result, then
		       ;; they all are.
		       ((find (svref form-num-trans 0) result :test #'eq)
			(setf (car (svref form-num-trans loc-num))
			      (wire:make-remote-object loc)))
		       ;; Otherwise, store these source-paths in the result.
		       (t
			(copy-stuff form-num-trans result)
			(setf (car (svref form-num-trans loc-num))
			      (wire:make-remote-object loc))))))
		   (t
		    (let ((translations (make-hash-table :test #'eq))
			  (form-num-trans (get-form-number-translations
					   d-source tlf-offset)))
		      (declare (simple-vector form-num-trans))
		      (setf (gethash d-source *source-info-offset-translations*)
			    translations)
		      (setf (gethash tlf-offset translations) form-num-trans)
		      (copy-stuff (convert-paths form-num-trans) result)
		      (setf (car (svref form-num-trans loc-num))
			    (wire:make-remote-object loc)))))))))))
      ;; Copy source-paths with code-locations from the result buffer to a
      ;; real result vector.
      (let* ((count (count-if #'(lambda (x) (wire:remote-object-p (car x)))
			      result))
	     (the-real-thing (make-array count))
	     (i -1))
	(declare (simple-vector the-real-thing)
		 (fixnum i count))
	(dotimes (j count)
	  (loop (if (wire:remote-object-p (car (aref result (incf i))))
		    (return)))
	  (setf (svref the-real-thing j) (aref result i)))
	the-real-thing))))

;;; GET-FORM-NUMBER-TRANSLATIONS -- Internal.
;;;
;;; This returns a vector of form-number translations to source-paths for
;;; d-source and the top-level-form indicated by the top-level-form offset.
;;;
(defun get-form-number-translations (d-source tlf-offset)
  (let ((name (debug-source-name d-source)))
    (ecase (debug-source-from d-source)
      (:file
       (cond
	((not (probe-file name))
	 (format t "~%Cannot set breakpoints for editor when source file no ~
		    longer exists:~%  ~A."
		 (namestring name)))
	(t
	 (let* ((local-tlf-offset (- tlf-offset
				     (debug-source-root-number d-source)))
		(char-offset
		 (aref (or (debug-source-start-positions d-source)
			   (error "Cannot set breakpoints for editor when ~
				   there is no start positions map."))
		       local-tlf-offset)))
	   (with-open-file (f name)
	     (cond
	      ((= (debug-source-created d-source) (file-write-date name))
	       (file-position f char-offset))
	      (t
	       (format t
		       "~%While setting a breakpoint for the editor, noticed ~
			source file has been modified since compilation:~%  ~A~@
			Using form offset instead of character position.~%"
		       (namestring name))
	       (dotimes (i local-tlf-offset) (read f))))
	     (form-number-translations (read f) tlf-offset))))))
      ((:lisp :stream)
       (form-number-translations (svref name tlf-offset) tlf-offset)))))

;;; SET-LOCATION-BREAKPOINT-FOR-EDITOR -- Internal Interface.
;;;
(defun set-location-breakpoint-for-editor (remote-obj-loc)
  "The editor calls this in the slave with a remote-object representing a
   code-location to set a breakpoint."
  (let ((loc (wire:remote-object-value remote-obj-loc)))
    (etypecase loc
      (interpreted-code-location
       (error "Breakpoints in interpreted code are currently unsupported."))
      (compiled-code-location
       (let* ((bpt (make-breakpoint #'(lambda (frame bpt)
					(declare (ignore frame bpt))
					(break "Editor installed breakpoint."))
				    loc))
	      (remote-bpt (wire:make-remote-object bpt)))
	 (activate-breakpoint bpt)
	 ;;(push remote-bpt (gethash name *editor-breakpoints*))
	 remote-bpt)))))

;;;
;;; Deleting breakpoints.
;;;

;;; DELETE-BREAKPOINT-FOR-EDITOR -- Internal Interface.
;;;
(defun delete-breakpoint-for-editor (remote-obj-bpt)
  "The editor calls this remotely in the slave to delete a breakpoint."
  (delete-breakpoint (wire:remote-object-value remote-obj-bpt))
  (wire:forget-remote-translation remote-obj-bpt))


;;;; Miscellaneous

;;; This appears here because it cannot go with the debug-function
;;; interface since DO-DEBUG-BLOCK-LOCATIONS isn't defined until after the
;;; debug-function routines.
;;;

;;; DEBUG-FUNCTION-START-LOCATION -- Public.
;;;
(defun debug-function-start-location (debug-fun)
  "Return a code-location before the body of $debug-fun and after all the
   arguments are in place if that location is can be determined, else
   return nil."
  (etypecase debug-fun
    (compiled-debug-function
     (code-location-from-pc debug-fun
			    (c::compiled-debug-function-start-pc
			     (compiled-debug-function-compiler-debug-fun
			      debug-fun))
			    nil))
    (interpreted-debug-function
     ;; Return the first location if there are any, otherwise nil.
     (handler-case (do-debug-function-blocks (block debug-fun nil)
		     (do-debug-block-locations (loc block nil)
		       (return-from debug-function-start-location loc)))
       (no-debug-blocks (condx)
	 (declare (ignore condx))
	 ())))))

(defun print-code-locations (function)
  (let ((debug-fun (function-debug-function function)))
    (do-debug-function-blocks (block debug-fun)
      (do-debug-block-locations (loc block)
	(fill-in-code-location loc)
	(format t "~S code location at ~D"
		(compiled-code-location-kind loc)
		(compiled-code-location-pc loc))
	(debug::print-code-location-source-form loc 0)
	(terpri)))))
