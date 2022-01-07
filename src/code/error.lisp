;;; Condition system.

(in-package "CONDITIONS")
(use-package "EXTENSIONS")
(use-package "KERNEL")

(in-package "KERNEL")
(export '(layout-invalid condition-function-name simple-control-error
	  simple-file-error simple-program-error simple-style-warning
	  simple-undefined-function))

(in-package "LISP")
(export '(break error warn cerror
	  ;;
	  ;; The following are found in Macros.Lisp:
	  check-type assert etypecase ctypecase ecase ccase
	  ;;
	  ;; These are all the new things to export from "LISP" now that this
	  ;; proposal has been accepted.
	  *break-on-signals* *debugger-hook* signal handler-case handler-bind
	  ignore-errors define-condition make-condition with-simple-restart
	  with-condition-restarts
	  restart-case restart-bind restart-name restart-name find-restart
	  compute-restarts invoke-restart invoke-restart-interactively abort
	  continue muffle-warning store-value use-value invoke-debugger restart
	  condition warning style-warning serious-condition simple-condition
	  simple-warning simple-error simple-condition-format-control
	  simple-condition-format-arguments storage-condition
	  type-error type-error-datum
	  type-error-expected-type simple-type-error program-error parse-error
	  control-error stream-error stream-error-stream end-of-file file-error
	  file-error-pathname cell-error cell-error-name unbound-variable
	  undefined-function
	  arithmetic-error arithmetic-error-operation arithmetic-error-operands
	  package-error package-error-package division-by-zero
	  floating-point-overflow floating-point-underflow
	  floating-point-inexact floating-point-invalid-operation))

(in-package "CONDITIONS")

;;;; Keyword utilities.

(eval-when (eval compile load)

(defun parse-keyword-pairs (list keys)
  (do ((l list (cddr l))
       (k '() (list* (cadr l) (car l) k)))
      ((or (null l) (not (member (car l) keys)))
       (values (nreverse k) l))))

(defmacro with-keyword-pairs ((names expression &optional keywords-var)
			      &body forms)
  (let ((temp (member '&rest names)))
    (unless (= (length temp) 2)
      (error "&rest keyword is ~:[missing~;misplaced~]." temp))
    (let ((key-vars (ldiff names temp))
          (key-var (or keywords-var (gensym)))
          (rest-var (cadr temp)))
      (let ((keywords (mapcar #'(lambda (x)
				  (intern (string x) ext:*keyword-package*))
			      key-vars)))
        `(multiple-value-bind (,key-var ,rest-var)
             (parse-keyword-pairs ,expression ',keywords)
           (let ,(mapcar #'(lambda (var keyword)
			     `(,var (getf ,key-var ,keyword)))
			 key-vars keywords)
	     ,@forms))))))

) ;eval-when



;;;; Restarts.

;;; A list of lists of restarts.
;;;
(defvar *restart-clusters* '())

;;;  An ALIST (condition . restarts) which records the restarts currently
;;; associated with Condition.
;;;
(defvar *condition-restarts* ())

(defun compute-restarts (&optional condition)
  "Return a list of all the currently active restarts ordered from most
   recently established to least recently established.  If Condition is
   specified, then only restarts associated with Condition (or with no
   condition) will be returned."
  (let ((associated ())
	(other ()))
    (dolist (alist *condition-restarts*)
      (if (eq (car alist) condition)
	  (setq associated (cdr alist))
	  (setq other (append (cdr alist) other))))
    (collect ((res))
      (dolist (restart-cluster *restart-clusters*)
	(dolist (restart restart-cluster)
	  (when (and (or (not condition)
			 (member restart associated)
			 (not (member restart other)))
		     (funcall (restart-test-function restart) condition))
	    (res restart))))
      (res))))


(defun restart-print (restart stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (print-unreadable-object (restart stream :type t :identity t))
      (restart-report restart stream)))

(defstruct (restart (:print-function restart-print))
  name
  function
  report-function
  interactive-function
  (test-function #'(lambda (cond) (declare (ignore cond)) t)))

(setf (documentation 'restart-name 'function)
      "Returns the name of the given restart object.")

(defun restart-report (restart stream)
  (funcall (or (restart-report-function restart)
               (let ((name (restart-name restart)))
		 #'(lambda (stream)
		     (if name (format stream "~S" name)
			      (format stream "~S" restart)))))
           stream))

(defmacro with-condition-restarts (condition-form restarts-form &body body)
  "WITH-CONDITION-RESTARTS Condition-Form Restarts-Form Form*
   Evaluates the Forms in a dynamic environment where the restarts in the list
   Restarts-Form are associated with the condition returned by Condition-Form.
   This allows FIND-RESTART, etc., to recognize restarts that are not related
   to the error currently being debugged.  See also RESTART-CASE."
  (let ((n-cond (gensym)))
    `(let ((*condition-restarts*
	    (cons (let ((,n-cond ,condition-form))
		    (cons ,n-cond
			  (append ,restarts-form
				  (cdr (assoc ,n-cond *condition-restarts*)))))
		  *condition-restarts*)))
       ,@body)))

(defmacro restart-bind (bindings &body forms)
  "Executes forms in a dynamic context where the given restart bindings are
   in effect.  Users probably want to use RESTART-CASE.  When clauses contain
   the same restart name, FIND-RESTART will find the first such clause."
  `(let ((*restart-clusters*
	  (cons (list
		 ,@(mapcar #'(lambda (binding)
			       (unless (or (car binding)
					   (member :report-function
						   binding :test #'eq))
				 (warn "Unnamed restart does not have a ~
					report function -- ~S"
				       binding))
			       `(make-restart
				 :name ',(car binding)
				 :function ,(cadr binding)
				 ,@(cddr binding)))
			       bindings))
		*restart-clusters*)))
     ,@forms))

(defun find-restart (name &optional condition)
  "Returns the first restart named name.  If name is a restart, it is returned
   if it is currently active.  If no such restart is found, nil is returned.
   It is an error to supply nil as a name.  If Condition is specified and not
   NIL, then only restarts associated with that condition (or with no
   condition) will be returned."
  (find-if #'(lambda (x)
	       (or (eq x name)
		   (eq (restart-name x) name)))
	   (compute-restarts condition)))

(defun invoke-restart (restart &rest values)
  "Calls the function associated with the given restart, passing any given
   arguments.  If the argument restart is not a restart or a currently active
   non-nil restart name, then a control-error is signalled."
  (let ((real-restart (find-restart restart)))
    (unless real-restart
      (error 'simple-control-error
	     :format-control "Restart ~S is not active."
	     :format-arguments (list restart)))
    (apply (restart-function real-restart) values)))

(defun invoke-restart-interactively (restart)
  "Calls the function associated with the given restart, prompting for any
   necessary arguments.  If the argument restart is not a restart or a
   currently active non-nil restart name, then a control-error is signalled."
  (let ((real-restart (find-restart restart)))
    (unless real-restart
      (error 'simple-control-error
	     :format-control "Restart ~S is not active."
	     :format-arguments (list restart)))
    (apply (restart-function real-restart)
	   (let ((interactive-function
		  (restart-interactive-function real-restart)))
	     (if interactive-function
		 (funcall interactive-function)
		 '())))))


(eval-when (compile load eval)
;;; Wrap the restart-case expression in a with-condition-restarts if
;;; appropriate.  Gross, but it's what the book seems to say...
;;;
(defun munge-restart-case-expression (expression data)
  (let ((exp (macroexpand expression)))
    (if (consp exp)
	(let* ((name (car exp))
	       (args (if (eq name 'cerror) (cddr exp) (cdr exp))))
	  (if (member name '(signal error cerror warn))
	      (once-only ((n-cond `(coerce-to-condition
				    ,(first args)
				    (list ,@(rest args))
				    ',(case name
					(warn 'simple-warning)
					(signal 'simple-condition)
					(t 'simple-error))
				    ',name)))
		`(with-condition-restarts
		     ,n-cond
		     (list ,@(mapcar #'(lambda (da)
					 `(find-restart ',(nth 0 da)))
				     data))
		   ,(if (eq name 'cerror)
			`(cerror ,(second expression) ,n-cond)
			`(,name ,n-cond))))
	      expression))
	expression)))

); eval-when (compile load eval)

(defmacro restart-case (expression &body clauses)
  "(RESTART-CASE form
   {(case-name arg-list {keyword value}* body)}*)
   The form is evaluated in a dynamic context where the clauses have special
   meanings as points to which control may be transferred (see INVOKE-RESTART).
   When clauses contain the same case-name, FIND-RESTART will find the first
   such clause.  If Expression is a call to SIGNAL, ERROR, CERROR or WARN (or
   macroexpands into such) then the signalled condition will be associated with
   the new restarts."
  (flet ((transform-keywords (&key report interactive test)
	   (let ((result '()))
	     (when report
	       (setq result (list* (if (stringp report)
				       `#'(lambda (stream)
					    (write-string ,report stream))
				       `#',report)
				   :report-function
				   result)))
	     (when interactive
	       (setq result (list* `#',interactive
				   :interactive-function
				   result)))
	     (when test
	       (setq result (list* `#',test
				   :test-function
				   result)))
	     (nreverse result))))
    (let ((block-tag (gensym))
	  (temp-var  (gensym))
	  (data
	   (mapcar #'(lambda (clause)
		       (with-keyword-pairs ((report interactive test
						    &rest forms)
					    (cddr clause))
			 (list (car clause) ;name=0
			       (gensym) ;tag=1
			       (transform-keywords :report report ;keywords=2
						   :interactive interactive
						   :test test)
			       (cadr clause) ;bvl=3  ;; FIX bvl means? body/block var list?
			       forms))) ;body=4
		   clauses)))
      `(block ,block-tag
	 (let ((,temp-var nil))
	   (tagbody
	    (restart-bind
		,(mapcar #'(lambda (datum)
			     (let ((name (nth 0 datum))
				   (tag  (nth 1 datum))
				   (keys (nth 2 datum)))
			       `(,name #'(lambda (&rest temp)
					   (setq ,temp-var temp)
					   (go ,tag))
				       ,@keys)))
			 data)
	      (return-from ,block-tag
			   ,(munge-restart-case-expression expression data)))
	    ,@(mapcan #'(lambda (datum)
			  (let ((tag  (nth 1 datum))
				(bvl  (nth 3 datum))
				(body (nth 4 datum)))
			    (list tag
				  `(return-from ,block-tag
						(apply #'(lambda ,bvl ,@body)
						       ,temp-var)))))
		      data)))))))


;;; If just one body form, then don't use progn.  This allows restart-case to
;;; "see" calls to error, etc.
;;;
(defmacro with-simple-restart ((restart-name format-string
					     &rest format-arguments)
			       &body forms)
  "(WITH-SIMPLE-RESTART (restart-name format-string format-arguments)
   body)
   If restart-name is not invoked, then all values returned by forms are
   returned.  If control is transferred to this restart, it immediately
   returns the values nil and t."
  `(restart-case ,(if (= (length forms) 1) (car forms) `(progn ,@forms))
     (,restart-name ()
        :report (lambda (stream)
		  (format stream ,format-string ,@format-arguments))
      (values nil t))))



;;;; Conditions.

(eval-when (compile load eval)

(defstruct (condition-class (:include slot-class))
  ;;
  ;; List of CONDITION-SLOT structures for the direct slots of this class.
  (slots nil :type list)
  ;;
  ;; List of CONDITION-SLOT structures for all of the effective class slots of
  ;; this class.
  (class-slots nil :type list)
  ;;
  ;; Report function or NIL.
  (report nil :type (or function null))
  ;;
  ;; List of alternating initargs and initforms.
  (default-initargs () :type list)
  ;;
  ;; CPL as a list of class objects, with all non-condition classes removed.
  (cpl () :type list)
  ;;
  ;; A list of all the effective instance allocation slots of this class that
  ;; have a non-constant initform or default-initarg.  Values for these slots
  ;; must be computed in the dynamic environment of MAKE-CONDITION.
  (hairy-slots nil :type list))

); eval-when (compile load eval)

(defstruct (condition
	    (:constructor make-condition-object (actual-initargs))
	    (:alternate-metaclass instance condition-class
				  make-condition-class))

  (function-name nil)
  ;;
  ;; Actual initargs supplied to MAKE-CONDITION.
  (actual-initargs (required-argument) :type list)
  ;;
  ;; Plist mapping slot names to any values that were assigned or defaulted
  ;; after creation.
  (assigned-slots () :type list))


(defstruct condition-slot
  (name (required-argument) :type symbol)
  ;;
  ;; List of all applicable initargs.
  (initargs (required-argument) :type list)
  ;;
  ;; Names of reader and writer functions.
  (readers (required-argument) :type list)
  (writers (required-argument) :type list)
  ;;
  ;; True if :INITFORM was specified.
  (initform-p (required-argument) :type (member t nil))
  ;;
  ;; If a function, call it with no args.  Otherwise, the actual value.
  (initform (required-argument) :type t)
  ;;
  ;; Allocation of this slot.  Nil only until defaulted.
  (allocation nil :type (member :instance :class nil))
  ;;
  ;; If :class allocation, a cons whose car holds the value.
  (cell nil :type (or cons null)))

(eval-when (compile load eval)
  (setf (condition-class-cpl (find-class 'condition))
	(list (find-class 'condition))))

(setf (condition-class-report (find-class 'condition))
      #'(lambda (cond stream)
	  (format stream "Condition ~S was signalled." (type-of cond))))

(eval-when (compile load eval)

(defun find-condition-layout (name parent-types)
  (let* ((cpl (remove-duplicates
	       (reverse
		(reduce #'append
			(mapcar #'(lambda (x)
				    (condition-class-cpl
				     (find-class x)))
				parent-types)))))
	 (cond-layout (info type compiler-layout 'condition))
	 (olayout (info type compiler-layout name))
	 (new-inherits
	  (order-layout-inherits (concatenate 'simple-vector
					      (layout-inherits cond-layout)
					      (mapcar #'class-layout cpl)))))
    (if (and olayout
	     (not (mismatch (layout-inherits olayout) new-inherits)))
	olayout
	(make-layout :class (make-undefined-class name)
		     :inherits new-inherits
		     :inheritance-depth -1
		     :length (layout-length cond-layout)))))

); EVAL-WHEN (COMPILE LOAD EVAL)


;;;; Condition reporting:

(defun %print-condition (s stream d)
  (declare (ignore d))
  (if *print-escape*
      (print-unreadable-object (s stream :identity t :type t))
      (dolist (class (condition-class-cpl (class-of s))
		     (error "No REPORT?  Shouldn't happen!"))
	(let ((report (condition-class-report class)))
	  (when report
	    (return (funcall report s stream)))))))

;;;; Condition slots:

(defvar *empty-slot* '(empty))

(defun find-slot-default (class slot)
  (let ((initargs (condition-slot-initargs slot))
	(cpl (condition-class-cpl class)))
    (dolist (class cpl)
      (let ((default-initargs (condition-class-default-initargs class)))
	(dolist (initarg initargs)
	  (let ((val (getf default-initargs initarg *empty-slot*)))
	    (unless (eq val *empty-slot*)
	      (return-from find-slot-default
			   (if (functionp val)
			       (funcall val)
			       val)))))))

    (if (condition-slot-initform-p slot)
	(let ((initform (condition-slot-initform slot)))
	  (if (functionp initform)
	      (funcall initform)
	      initform))
	(error "Condition slot is not bound: ~S"
	       (condition-slot-name slot)))))

(defun find-slot (classes name)
  (dolist (sclass classes nil)
    (dolist (slot (condition-class-slots sclass))
      (when (eq (condition-slot-name slot) name)
	(return-from find-slot slot)))))

(defun condition-writer-function (condition new-value name)
  (dolist (cslot (condition-class-class-slots
		  (layout-class (%instance-layout condition)))
		 (setf (getf (condition-assigned-slots condition) name)
		       new-value))
    (when (eq (condition-slot-name cslot) name)
      (return (setf (car (condition-slot-cell cslot)) new-value)))))

(defun condition-reader-function (condition name)
  (let ((class (layout-class (%instance-layout condition))))
    (dolist (cslot (condition-class-class-slots class))
      (when (eq (condition-slot-name cslot) name)
	(return-from condition-reader-function
		     (car (condition-slot-cell cslot)))))

    (let ((val (getf (condition-assigned-slots condition) name
		     *empty-slot*)))
      (if (eq val *empty-slot*)
	  (let ((actual-initargs (condition-actual-initargs condition))
		(slot (find-slot (condition-class-cpl class) name)))
	    (dolist (initarg (condition-slot-initargs slot))
	      (let ((val (getf actual-initargs initarg *empty-slot*)))
		(unless (eq val *empty-slot*)
		  (return-from condition-reader-function
			       (setf (getf (condition-assigned-slots condition)
					   name)
				     val)))))
	    (setf (getf (condition-assigned-slots condition) name)
		  (find-slot-default class slot)))
	  val))))


(defun make-condition (thing &rest args)
  "Make an instance of a condition object using the specified initargs."
  ;; Note: ANSI specifies no exceptional situations in this function.
  ;; signalling simple-type-error would not be wrong.
  (let* ((thing (if (symbolp thing)
		    (find-class thing)
		    thing))
	 (class (typecase thing
		  (condition-class thing)
		  (class
		   (error 'simple-type-error
			  :datum thing
			  :expected-type 'condition-class
			  :format-control "~S is not a condition class."
			  :format-arguments (list thing)))
		  (t
		   (error 'simple-type-error
			  :datum thing
			  :expected-type 'condition-class
			  :format-control "Bad thing for class arg:~%  ~S"
			  :format-arguments (list thing)))))
	 (res (make-condition-object args)))
    (setf (%instance-layout res) (class-layout class))
    ;;
    ;; Set any class slots with initargs present in this call.
    (dolist (cslot (condition-class-class-slots class))
      (dolist (initarg (condition-slot-initargs cslot))
	(let ((val (getf args initarg *empty-slot*)))
	  (unless (eq val *empty-slot*)
	    (setf (car (condition-slot-cell cslot)) val)))))
    ;;
    ;; Default any slots with non-constant defaults now.
    (dolist (hslot (condition-class-hairy-slots class))
      (when (dolist (initarg (condition-slot-initargs hslot) t)
	      (unless (eq (getf args initarg *empty-slot*) *empty-slot*)
		(return nil)))
	(setf (getf (condition-assigned-slots res) (condition-slot-name hslot))
	      (find-slot-default class hslot))))

    res))


;;;; DEFINE-CONDITION

(eval-when (compile load eval)
(defun %compiler-define-condition (name direct-supers layout)
  (multiple-value-bind (class old-layout)
		       (insured-find-class name #'condition-class-p
					   #'make-condition-class)
    (setf (layout-class layout) class)
    (setf (class-direct-superclasses class)
	  (mapcar #'find-class direct-supers))
    (cond ((not old-layout)
	   (register-layout layout))
	  ((not *type-system-initialized*)
	   (setf (layout-class old-layout) class)
	   (setq layout old-layout)
	   (unless (eq (class-layout class) layout)
	     (register-layout layout)))
	  ((redefine-layout-warning old-layout "current"
				    layout "new")
	   (register-layout layout :invalidate t))
	  ((not (class-layout class))
	   (register-layout layout)))

    (setf (layout-info layout)
	  (layout-info (class-layout (find-class 'condition))))

    (setf (find-class name) class)
    ;;
    ;; Initialize CPL slot.
    (setf (condition-class-cpl class)
	  (remove-if-not #'condition-class-p
			 (std-compute-class-precedence-list class))))
  (undefined-value))

); eval-when (compile load eval)


;;; COMPUTE-EFFECTIVE-SLOTS  --  Internal
;;;
;;;   Compute the effective slots of class, copying inherited slots and
;;; side-effecting direct slots.
;;;
(defun compute-effective-slots (class)
  (collect ((res (copy-list (condition-class-slots class))))
    (dolist (sclass (condition-class-cpl class))
      (dolist (sslot (condition-class-slots sclass))
	(let ((found (find (condition-slot-name sslot) (res)
			   :test #'eq)))
	  (cond (found
		 (setf (condition-slot-initargs found)
		       (union (condition-slot-initargs found)
			      (condition-slot-initargs sslot)))
		 (unless (condition-slot-initform-p found)
		   (setf (condition-slot-initform-p found)
			 (condition-slot-initform-p sslot))
		   (setf (condition-slot-initform found)
			 (condition-slot-initform sslot)))
		 (unless (condition-slot-allocation found)
		   (setf (condition-slot-allocation found)
			 (condition-slot-allocation sslot))))
		(t
		 (res (copy-structure sslot)))))))
    (res)))


(defun %define-condition (name slots documentation report default-initargs)
  (let ((class (find-class name)))
    (setf (slot-class-print-function class) #'%print-condition)
    (setf (condition-class-slots class) slots)
    (setf (condition-class-report class) report)
    (setf (condition-class-default-initargs class) default-initargs)
    (setf (documentation name 'type) documentation)

    (dolist (slot slots)
      ;;
      ;; Set up reader & writer functions.
      (let ((name (condition-slot-name slot)))
	(dolist (reader (condition-slot-readers slot))
	  (setf (fdefinition reader)
		#'(lambda (condition)
		    (condition-reader-function condition name))))
	(dolist (writer (condition-slot-writers slot))
	  (setf (fdefinition writer)
		#'(lambda (new-value condition)
		    (condition-writer-function condition new-value name))))))

    ;;
    ;; Compute effective slots and set up the class and hairy slots (subsets of
    ;; the effective slots.)
    (let ((eslots (compute-effective-slots class))
	  (e-def-initargs
	   (reduce #'append
		   (mapcar #'condition-class-default-initargs
			   (condition-class-cpl class)))))
      (dolist (slot eslots)
	(ecase (condition-slot-allocation slot)
	  (:class
	   (unless (condition-slot-cell slot)
	     (setf (condition-slot-cell slot)
		   (list (if (condition-slot-initform-p slot)
			     (let ((initform (condition-slot-initform slot)))
			       (if (functionp initform)
				   (funcall initform)
				   initform))
			     *empty-slot*))))
	   (push slot (condition-class-class-slots class)))
	  ((:instance nil)
	   (setf (condition-slot-allocation slot) :instance)
	   (when (or (functionp (condition-slot-initform slot))
		     (dolist (initarg (condition-slot-initargs slot) nil)
		       (when (functionp (getf e-def-initargs initarg))
			 (return t))))
	     (push slot (condition-class-hairy-slots class))))))))
  name)


(defmacro define-condition (name (&rest parent-types) (&rest slot-specs)
				 &body options)
  "DEFINE-CONDITION Name (Parent-Type*) (Slot-Spec*) Option*
   Define NAME as a condition type.  This new type inherits slots and its
   report function from the specified PARENT-TYPEs.  A slot spec is a list of:
     (slot-name :reader <rname> :initarg <iname> {Option Value}*

   The DEFINE-CLASS slot options :ALLOCATION, :INITFORM, [slot] :DOCUMENTATION
   and :TYPE and the overall options :DEFAULT-INITARGS and
   [type] :DOCUMENTATION are also allowed.

   The :REPORT option is peculiar to DEFINE-CONDITION.  Its argument is either
   a string or a two-argument lambda or function name.  If a function, the
   function is called with the condition and stream to report the condition.
   If a string, the string is printed.

   Condition types are classes, but (as allowed by ANSI and not as described in
   CLtL2) are neither STANDARD-OBJECTs nor STRUCTURE-OBJECTs.  WITH-SLOTS and
   SLOT-VALUE may not be used on condition objects."
  (let* ((parent-types (or parent-types '(condition)))
	 (layout (find-condition-layout name parent-types))
	 (documentation nil)
	 (report nil)
	 (default-initargs ()))
    (collect ((slots)
	      (all-readers nil append)
	      (all-writers nil append))
      (dolist (spec slot-specs)
	(when (keywordp spec)
	  (warn "Keyword slot name indicates probable syntax error:~%  ~S"
		spec))
	(let* ((spec (if (consp spec) spec (list spec)))
	       (slot-name (first spec))
	       (allocation :instance)
	       (initform-p nil)
	       initform)
	  (collect ((initargs)
		    (readers)
		    (writers))
	    (do ((options (rest spec) (cddr options)))
		((null options))
	      (unless (and (consp options) (consp (cdr options)))
		(error "Malformed condition slot spec:~%  ~S." spec))
	      (let ((arg (second options)))
		(case (first options)
		  (:reader (readers arg))
		  (:writer (writers arg))
		  (:accessor
		   (readers arg)
		   (writers `(setf ,arg)))
		  (:initform
		   (when initform-p
		     (error "More than one :INITFORM in:~%  ~S" spec))
		   (setq initform-p t)
		   (setq initform arg))
		  (:initarg (initargs arg))
		  (:allocation
		   (setq allocation arg))
		  (:type)
		  (t
		   (error "Unknown slot option:~%  ~S" (first options))))))

	    (unless (or (initargs) initform-p)
	      (warn "Probable error: no initargs or initform for condition ~
		     slot:~%  ~S"
		    slot-name))
	    (unless (readers)
	      (warn "Probable error: no readers for condition slot:~%  ~S"
		    slot-name))

	    (all-readers (readers))
	    (all-writers (writers))
	    (slots `(make-condition-slot
		     :name ',slot-name
		     :initargs ',(initargs)
		     :readers ',(readers)
		     :writers ',(writers)
		     :initform-p ',initform-p
		     :initform
		     ,(if (constantp initform)
			  `',(eval initform)
			  `#'(lambda () ,initform)))))))

      (dolist (option options)
	(unless (consp option)
	  (error "Bad option:~%  ~S" option))
	(case (first option)
	  (:documentation (setq documentation (second option)))
	  (:report
	   (let ((arg (second option)))
	     (setq report
		   (if (stringp arg)
		       `#'(lambda (condition stream)
			    (declare (ignore condition))
			    (write-string ,arg stream))
		       `#'(lambda (condition stream)
			    (funcall #',arg condition stream))))))
	  (:default-initargs
	   (do ((initargs (rest option) (cddr initargs)))
	       ((endp initargs))
	     (let ((val (second initargs)))
	       (setq default-initargs
		     (list* `',(first initargs)
			    (if (constantp val)
				`',(eval val)
				`#'(lambda () ,val))
			    default-initargs)))))
	  (t
	   (error "Unknown option: ~S" (first option)))))

      (when (all-writers)
	(warn "Condition slot setters probably not allowed in ANSI CL:~%  ~S"
	      (all-writers)))

      `(progn
	 (eval-when (compile load eval)
	   (%compiler-define-condition ',name ',parent-types ',layout))

	 (declaim (ftype (function (t) t) ,@(all-readers)))
	 (declaim (ftype (function (t t) t) ,@(all-writers)))

	 (%define-condition ',name
			    (list ,@(slots))
			    ,documentation
			    ,report
			    (list ,@default-initargs))))))


;;;; HANDLER-BIND (SIGNAL is in lispinit.lisp).

(defvar *handler-clusters* nil)

(defmacro handler-bind (bindings &body forms)
  "(HANDLER-BIND ( {(type handler)}* )  body)
   Executes body in a dynamic context where the given handler bindings are
   in effect.  Each handler must take the condition being signalled as an
   argument.  The bindings are searched first to last in the event of a
   signalled condition."
  (unless (every #'(lambda (x) (and (listp x) (= (length x) 2))) bindings)
    (error "Ill-formed handler bindings."))
  `(let ((*handler-clusters*
	  (cons (list ,@(mapcar #'(lambda (x) `(cons ',(car x) ,(cadr x)))
				bindings))
		*handler-clusters*)))
     (multiple-value-prog1
      (progn ,@forms)
      ;; Wait for any float exceptions
      #+x86 (float-wait))))


;;;; Condition definitions.

(define-condition serious-condition (condition) ())

(define-condition error (serious-condition) ())

(define-condition warning (condition) ())
(define-condition style-warning (warning) ())

(defun simple-condition-printer (condition stream)
  (apply #'format stream (simple-condition-format-control condition)
	 		 (simple-condition-format-arguments condition)))

(define-condition simple-condition ()
  ((format-control :reader simple-condition-format-control
		   :initarg :format-control)
   (format-arguments :reader simple-condition-format-arguments
		     :initarg :format-arguments
		     :initform '()))
  (:report simple-condition-printer))

(define-condition simple-warning (simple-condition warning) ())
(define-condition simple-style-warning (simple-condition style-warning) ())

(defun print-simple-error (condition stream)
  (format stream "~&~@<Error in function ~S:  ~3i~:_~?~:>"
	  (condition-function-name condition)
	  (simple-condition-format-control condition)
	  (simple-condition-format-arguments condition)))

(define-condition simple-error (simple-condition error) ()
  ;; This is the condition type used by error and cerror when
  ;; a format-control string is supplied as the first argument.
  (:report print-simple-error))

(define-condition storage-condition (serious-condition) ())

(define-condition type-error (error)
  ((datum :reader type-error-datum :initarg :datum)
   (expected-type :reader type-error-expected-type :initarg :expected-type))
  (:report
   (lambda (condition stream)
     (format stream "~@<Type-error in ~S:  ~3i~:_~S is not of type ~S~:>"
	     (condition-function-name condition)
	     (type-error-datum condition)
	     (type-error-expected-type condition)))))

(define-condition simple-type-error (simple-condition type-error) ())

(define-condition kernel:layout-invalid (type-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Layout-invalid error in ~S:~@
		     Type test of class ~S was passed obsolete instance:~%  ~S"
	     (condition-function-name condition)
	     (kernel:class-proper-name (type-error-expected-type condition))
	     (type-error-datum condition)))))

(define-condition case-failure (type-error)
  ((name :reader case-failure-name :initarg :name)
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:report
    (lambda (condition stream)
      (format stream "~@<~S fell through ~S expression.  ~:_Wanted one of ~:S.~:>"
	      (type-error-datum condition)
	      (case-failure-name condition)
	      (case-failure-possibilities condition)))))


(define-condition program-error (error) ())
(define-condition parse-error   (error) ())
(define-condition control-error (error) ())
(define-condition stream-error  (error)
  ((stream :reader stream-error-stream :initarg :stream)))

(define-condition end-of-file (stream-error) ()
  (:report
   (lambda (condition stream)
     (format stream "End-of-File on ~S"
	     (stream-error-stream condition)))))

(define-condition file-error (error)
  ((pathname :reader file-error-pathname :initarg :pathname)))

;;; INTERNAL
(define-condition simple-program-error (simple-condition program-error)())
(define-condition simple-control-error (simple-condition control-error)())

(define-condition simple-file-error (simple-condition file-error) ()
  (:report
   (lambda (condition stream)
     (format stream "~&~@<File-error in function ~S:  ~3i~:_~?~:>"
	     (condition-function-name condition)
	     (simple-condition-format-control condition)
	     (simple-condition-format-arguments condition)))))

(define-condition package-error (error)
  ((package :reader package-error-package :initarg :package)))

(define-condition cell-error (error)
  ((name :reader cell-error-name :initarg :name)))

(define-condition unbound-variable (cell-error) ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Error in ~S:  the variable ~S is unbound."
	     (condition-function-name condition)
	     (cell-error-name condition)))))

(define-condition undefined-function (cell-error) ()
  (:report
   (lambda (condition stream)
     (format stream
	     "Error in ~S:  the function ~S is undefined."
	     (condition-function-name condition)
	     (cell-error-name condition)))))

(define-condition simple-undefined-function (simple-condition
					     undefined-function) ())

(define-condition arithmetic-error (error)
  ((operation :reader arithmetic-error-operation :initarg :operation
	      :initform nil)
   (operands :reader arithmetic-error-operands :initarg :operands))
  (:report (lambda (condition stream)
	     (format stream "Arithmetic error ~S signalled."
		     (type-of condition))
	     (when (arithmetic-error-operation condition)
	       (format stream "~%Operation was ~S, operands ~S."
		       (arithmetic-error-operation condition)
		       (arithmetic-error-operands condition))))))

(define-condition division-by-zero         (arithmetic-error) ())
(define-condition floating-point-overflow  (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())
(define-condition floating-point-inexact   (arithmetic-error) ())
(define-condition floating-point-invalid-operation   (arithmetic-error) ())

;;; This condition is signalled whenever we make a UNKNOWN-TYPE so that
;;; compiler warnings can be emitted as appropriate.
;;;
(define-condition parse-unknown-type (condition)
  ((specifier :reader parse-unknown-type-specifier :initarg :specifier)))


;;;; HANDLER-CASE and IGNORE-ERRORS.

#|
;;; Version used in older versions of CMUCL which had lossage closing over
;;; tags.
;;;
(defmacro handler-case (form &rest cases)
  "(HANDLER-CASE form
   { (type ([var]) body) }* )
   Executes form in a context with handlers established for the condition
   types.  A peculiar property allows type to be :no-error.  If such a clause
   occurs, and form returns normally, all its values are passed to this clause
   as if by MULTIPLE-VALUE-CALL.  The :no-error clause accepts more than one
   var specification."
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
	(let ((normal-return (make-symbol "normal-return"))
	      (error-return  (make-symbol "error-return")))
	  `(block ,error-return
	     (multiple-value-call #'(lambda ,@(cdr no-error-clause))
	       (block ,normal-return
		 (return-from ,error-return
		   (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let ((var (gensym))
	      (outer-tag (gensym))
	      (inner-tag (gensym))
	      (tag-var (gensym))
	      (annotated-cases (mapcar #'(lambda (case) (cons (gensym) case))
				       cases)))
	  `(let ((,outer-tag (cons nil nil))
		 (,inner-tag (cons nil nil))
		 ,var ,tag-var)
	     (declare (ignorable ,var))
	     (catch ,outer-tag
	       (catch ,inner-tag
		 (throw ,outer-tag
			(handler-bind
			    ,(mapcar #'(lambda (annotated-case)
					 `(,(cadr annotated-case)
					   #'(lambda (temp)
					       ,(if (caddr annotated-case)
						    `(setq ,var temp)
						    '(declare (ignore temp)))
					       (setf ,tag-var
						     ',(car annotated-case))
					       (throw ,inner-tag nil))))
				     annotated-cases)
			  ,form)))
	       (case ,tag-var
		 ,@(mapcar #'(lambda (annotated-case)
			       (let ((body (cdddr annotated-case))
				     (varp (caddr annotated-case)))
				 `(,(car annotated-case)
				   ,@(if varp
					 `((let ((,(car varp) ,var))
					     ,@body))
					 body))))
			   annotated-cases))))))))
|#

;;; This macro doesn't work in older version of CMUCL system due to lossage
;;; in closing over tags.  The previous version sets up unique run-time
;;; tags.
;;;
;; FIX seems to be run after stack returned
;;       (whereas handler-bind runs with the stack at the point of error)
;;       eg have to handler-bind around %command-loop in ed to get backtrace
(defmacro handler-case (form &rest cases)
  "(HANDLER-CASE form { (type ([var]) body) }* )
   Executes form in a context with handlers established for the condition
   types.  A peculiar property allows type to be :no-error.  If such a clause
   occurs, and form returns normally, all its values are passed to this clause
   as if by MULTIPLE-VALUE-CALL.  The :no-error clause accepts more than one
   var specification."
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
	(let ((normal-return (make-symbol "normal-return"))
	      (error-return  (make-symbol "error-return")))
	  `(block ,error-return
	     (multiple-value-call #'(lambda ,@(cdr no-error-clause))
	       (block ,normal-return
		 (return-from ,error-return
		   (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let ((tag (gensym))
	      (var (gensym))
	      (annotated-cases (mapcar #'(lambda (case) (cons (gensym) case))
				       cases)))
	  `(block ,tag
	     (let ((,var nil))
	       (declare (ignorable ,var))
	       (tagbody
		 (handler-bind
		  ,(mapcar #'(lambda (annotated-case)
			       (list (cadr annotated-case)
				     `#'(lambda (temp)
					  ,(if (caddr annotated-case)
					       `(setq ,var temp)
					       '(declare (ignore temp)))
					  (go ,(car annotated-case)))))
			   annotated-cases)
			       (return-from ,tag ,form))
		 ,@(mapcan
		    #'(lambda (annotated-case)
			(list (car annotated-case)
			      (let ((body (cdddr annotated-case)))
				`(return-from
				  ,tag
				  ,(cond ((caddr annotated-case)
					  `(let ((,(caaddr annotated-case)
						  ,var))
					     ,@body))
					 ((not (cdr body))
					  (car body))
					 (t
					  `(progn ,@body)))))))
			   annotated-cases))))))))

(defmacro ignore-errors (&rest forms)
  "Execute forms after establishing a handler for all error conditions.  On
   error the handler returns nil and the signalled condition."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))



;;;; Restart definitions.

(define-condition abort-failure (control-error) ()
  (:report
   "Found an \"abort\" restart that failed to transfer control dynamically."))

;;; ABORT signals an error in case there was a restart named abort that did
;;; not tranfer control dynamically.  This could happen with RESTART-BIND.
;;;
(defun abort (&optional condition)
  "Transfers control to a restart named abort, signalling a control-error if
   none exists."
  (invoke-restart (find-restart 'abort condition))
  (error 'abort-failure))


(defun muffle-warning (&optional condition)
  "Transfers control to a restart named muffle-warning, signalling a
   control-error if none exists."
  (invoke-restart (find-restart 'muffle-warning condition)))


;;; DEFINE-NIL-RETURNING-RESTART finds the restart before invoking it to keep
;;; INVOKE-RESTART from signalling a control-error condition.
;;;
(defmacro define-nil-returning-restart (name args doc)
  `(defun ,name (,@args &optional condition)
     ,doc
     (if (find-restart ',name condition) (invoke-restart ',name ,@args))))

(define-nil-returning-restart continue ()
  "Transfer control to a restart named continue, returning nil if none exists.")

(define-nil-returning-restart store-value (value)
  "Transfer control and value to a restart named store-value, returning nil if
   none exists.")

(define-nil-returning-restart use-value (value)
  "Transfer control and value to a restart named use-value, returning nil if
   none exists.")
