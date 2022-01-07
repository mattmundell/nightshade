;;; Low-level Unix support for MACH-only features.

(in-package "MACH")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "SYSTEM")

(export '(port mach-task_self mach-task_data mach-task_notify
	  kern-success get-mach-error-msg
	  gr-error gr-call gr-call* gr-bind
	  vm_allocate vm_copy vm_deallocate vm_statistics))


;;;; Standard ports.

(def-alien-type port int)

(def-alien-routine ("task_self" mach-task_self) port)
(def-alien-routine ("thread_reply" mach-task_data) port)
(def-alien-routine ("task_notify" mach-task_notify) port)


#[ Making Sense of Mach Return Codes

Whenever a remote procedure call returns a Unix error code (such as
\code{kern_return_t}), it is usually prudent to check that code to see if
the call was successful.  To relieve the programmer of testing this value,
and to centralize the information about the meaning of non-success return
codes, there are a number of macros and functions.  The function
`get-unix-error-msg' is related.

{function:mach:gr-error}
{function:mach:gr-call}
{function:mach:gr-call*}
{function:mach:gr-bind}
]#


;;;; Return codes.

(def-alien-type kern-return int)

(defconstant kern-success 0)
(defconstant kern-invalid-address 1)
(defconstant kern-protection-failure 2)
(defconstant kern-no-space 3)
(defconstant kern-invalid-argument 4)
(defconstant kern-failure 5)
(defconstant kern-resource-shortage 6)
(defconstant kern-not-receiver 7)
(defconstant kern-no-access 8)
(defconstant kern-memory-failure 9)
(defconstant kern-memory-error 10)
(defconstant kern-already-in-set 11)
(defconstant kern-not-in-set 12)
(defconstant kern-name-exists 13)
(defconstant kern-aborted 14)
(defconstant kern-memory-present 23)

(def-alien-routine ("mach_error_string" get-mach-error-msg) c-string
  (errno kern-return))

;;; GR-Error  --  Public
;;;
(defun gr-error (function gr &optional context)
  "Signal an error, printing a message indicating that the call to the
   $function failed, with the return code $gr.  If supplied, the print the
   string $context after the $function name and before the string
   associated with the $gr.  For example:

     (gr-error 'nukegarbage 3 \"lost big\")

	 Error in function GR-ERROR:
	 NUKEGARBAGE lost big, no space.
	 Proceed cases:
	 0: Return to Top-Level.
	 Debug  (type H for help)
	 (Signal #<Conditions:Simple-Error.5FDE0>)
	 0]"
  (or (eql gr kern-success)
      (error "~S~@[ ~A~], ~(~A~)." function context (get-mach-error-msg gr))))

;;; GR-Call  --  Public
;;;
(defmacro gr-call (fun &rest args)
  "gr-call function {arg}*

   Call $fun with $args and signal an error if the first returned value
   (the GeneralReturn code) indicates an error, else return ().

   For example

     (gr-call mach:port_allocate *task-self*) => ()"
  (let ((n-gr (gensym)))
    `(let ((,n-gr (,fun ,@args)))
       (or (eql ,n-gr kern-success) (gr-error ',fun ,n-gr)))))

;; FIX gr-call2?
;;; GR-Call*  --  Public
;;;
(defmacro gr-call* (fun &rest args)
  "gr-call* function {arg}*

   Call $fun with $args and signal an error if the first returned value
   (the GeneralReturn code) indicates an error, else return the second
   value returned from the call."
  (let ((n-gr (gensym))
	(n-res (gensym)))
    `(multiple-value-bind (,n-gr ,n-res) (,fun ,@args)
       (or (eql ,n-gr kern-success) (gr-error ',fun ,n-gr))
       ,n-res)))

;;; GR-Bind  --  Public
;;;
(defmacro gr-bind (vars (fun . args) &body (body decls))
  "gr-bind ({var}*) (function {arg}*) {form}*


   Bind the vars listed in $vars from the second variable to the values
   resulting from calling $function with $args, as in
   `multiple-value-bind'.

   If the first returned value (the GeneralReturn code) indicates an error,
   then signal an error, else return ().

   For example

     (gr-bind (port_list port_list_cnt)
              (mach:port_select *task-self*)
       (format t \"The port count is ~S.\" port_list_cnt)
       port_list)"
  (let ((n-gr (gensym)))
    `(multiple-value-bind (,n-gr ,@vars) (,fun ,@args)
       ,@decls
       (or (eql ,n-gr kern-success) (gr-error ',fun ,n-gr))
       ,@body)))


;;;; VM routines.

(export '(vm_allocate vm_copy vm_deallocate vm_statistics))

(def-alien-routine ("vm_allocate" vm_allocate) int
  (task port)
  (address system-area-pointer :in-out)
  (size unsigned-long)
  (anywhere boolean))

(def-alien-routine ("vm_copy" vm_copy) int
  (task port)
  (source system-area-pointer)
  (count unsigned-long)
  (dest system-area-pointer))

(def-alien-routine ("vm_deallocate" vm_deallocate) int
  (task port)
  (address system-area-pointer)
  (size unsigned-long))

(def-alien-type nil
  (struct vm_statistics
    (pagesize long)
    (free_count long)
    (active_count long)
    (inactive_count long)
    (wire_count long)
    (zero_fill_count long)
    (reactivations long)
    (pageins long)
    (pageouts long)
    (faults long)
    (cow_faults long)
    (lookups long)
    (hits long)))

(defun vm_statistics (task)
  (with-alien ((vm_stats (struct vm_statistics)))
    (values
     (alien-funcall (extern-alien "vm_statistics"
				  (function int
					    port
					    (* (struct vm_statistics))))
		    task (alien-sap vm_stats))
     (slot vm_stats 'pagesize)
     (slot vm_stats 'free_count)
     (slot vm_stats 'active_count)
     (slot vm_stats 'inactive_count)
     (slot vm_stats 'wire_count)
     (slot vm_stats 'zero_fill_count)
     (slot vm_stats 'reactivations)
     (slot vm_stats 'pageins)
     (slot vm_stats 'pageouts)
     (slot vm_stats 'faults)
     (slot vm_stats 'cow_faults)
     (slot vm_stats 'lookups)
     (slot vm_stats 'hits))))
