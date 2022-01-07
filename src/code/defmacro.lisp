;;; -*- Log: code.log; Mode: Lisp; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /home/CVS-cmucl/src/code/defmacro.lisp,v 1.20 1997/01/18 14:31:00 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Blaine Burks.
;;;
(in-package "LISP")


;;;; Some variable definitions.

;;; Variables for amassing the results of parsing a defmacro.  Declarations
;;; in DEFMACRO are the reason this isn't as easy as it sounds.
;;;
(defvar *arg-tests* ()
  "A list of tests that do argument counting at expansion time.")

(defvar *system-lets* ()
  "Let bindings that are done to make lambda-list parsing possible.")

(defvar *user-lets* ()
  "Let bindings that the user has explicitly supplied.")

(defvar *default-default* nil
  "Unsupplied optional and keyword arguments get this value defaultly.")

;; Temps that we introduce and might not reference.
(defvar *ignorable-vars*)
  


;;;; Stuff to parse DEFMACRO, MACROLET, DEFINE-SETF-METHOD, and DEFTYPE.

;;; PARSE-DEFMACRO returns, as multiple-values, a body, possibly a declare
;;; form to put where this code is inserted, and the documentation for the
;;; parsed body.
;;;
(defun parse-defmacro (lambda-list arg-list-name code name error-kind
				   &key (annonymousp nil)
				   (doc-string-allowed t)
				   ((:environment env-arg-name))
				   ((:default-default *default-default*))
				   (error-fun 'error))
  "Returns as multiple-values a parsed body, any local-declarations that
   should be made where this body is inserted, and a doc-string if there is
   one."
  (multiple-value-bind (body declarations documentation)
		       (parse-body code nil doc-string-allowed)
    (let* ((*arg-tests* ())
	   (*user-lets* ())
	   (*system-lets* ())
	   (*ignorable-vars* ()))
      (multiple-value-bind
	  (env-arg-used minimum maximum)
	  (parse-defmacro-lambda-list lambda-list arg-list-name name
				      error-kind error-fun (not annonymousp)
				      nil env-arg-name)
	(values
	 `(let* ,(nreverse *system-lets*)
	   ,@(when *ignorable-vars*
	       `((declare (ignorable ,@*ignorable-vars*))))
	    ,@*arg-tests*
	    (let* ,(nreverse *user-lets*)
	      ,@declarations
	      ,@body))
	 `(,@(when (and env-arg-name (not env-arg-used))
	       `((declare (ignore ,env-arg-name)))))
	 documentation
	 minimum
	 maximum)))))


(defun parse-defmacro-lambda-list
       (lambda-list arg-list-name name error-kind error-fun
		    &optional top-level env-illegal env-arg-name)
  (let ((path (if top-level `(cdr ,arg-list-name) arg-list-name))
	(now-processing :required)
	(maximum 0)
	(minimum 0)
	(keys ())
	rest-name restp allow-other-keys-p env-arg-used)
    ;; This really strange way to test for '&whole is neccessary because member
    ;; does not have to work on dotted lists, and dotted lists are legal
    ;; in lambda-lists.
    (when (and (do ((list lambda-list (cdr list)))
		   ((atom list) nil)
		 (when (eq (car list) '&whole) (return t)))
	       (not (eq (car lambda-list) '&whole)))
      (error "&Whole must appear first in ~S lambda-list." error-kind))
    (do ((rest-of-args lambda-list (cdr rest-of-args)))
	((atom rest-of-args)
	 (cond ((null rest-of-args) nil)
	       ;; Varlist is dotted, treat as &rest arg and exit.
	       (t (push-let-binding rest-of-args path nil)
		  (setf restp t))))
      (let ((var (car rest-of-args)))
	(cond ((eq var '&whole)
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (push-let-binding (car rest-of-args) arg-list-name nil))
		     (t
		      (defmacro-error "&WHOLE" error-kind name))))
	      ((eq var '&environment)
	       (cond (env-illegal
		      (error "&Environment not valid with ~S." error-kind))
		     ((not top-level)
		      (error "&Environment only valid at top level of ~
		      lambda-list.")))
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (push-let-binding (car rest-of-args) env-arg-name nil)
		      (setf env-arg-used t))
		     (t
		      (defmacro-error "&ENVIRONMENT" error-kind name))))
	      ((or (eq var '&rest) (eq var '&body))
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (setf restp t)
		      (push-let-binding (car rest-of-args) path nil))
		     ;;
		     ;; This branch implements an incompatible extension to
		     ;; Common Lisp.  In place of a symbol following &body,
		     ;; there may be a list of up to three elements which will
		     ;; be bound to the body, declarations, and doc-string of
		     ;; the body.
		     ((and (cdr rest-of-args)
			   (consp (cadr rest-of-args))
			   (symbolp (caadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (setf restp t)
		      (let ((body-name (caar rest-of-args))
			    (declarations-name (cadar rest-of-args))
			    (doc-string-name (caddar rest-of-args))
			    (parse-body-values (gensym)))
			(push-let-binding
			 parse-body-values
			 `(multiple-value-list
			   (parse-body ,path ,env-arg-name
				       ,(not (null doc-string-name))))
			 t)
			(setf env-arg-used t)
			(when body-name
			  (push-let-binding body-name
					    `(car ,parse-body-values) nil))
			(when declarations-name
			  (push-let-binding declarations-name
					    `(cadr ,parse-body-values) nil))
			(when doc-string-name
			  (push-let-binding doc-string-name
					    `(caddr ,parse-body-values) nil))))
		     (t
		      (defmacro-error (symbol-name var) error-kind name))))
	      ((eq var '&optional)
	       (setf now-processing :optionals))
	      ((eq var '&key)
	       (setf now-processing :keywords)
	       (setf rest-name (gensym "KEYWORDS-"))
	       (push rest-name *ignorable-vars*)
	       (setf restp t)
	       (push-let-binding rest-name path t))
	      ((eq var '&allow-other-keys)
	       (setf allow-other-keys-p t))
	      ((eq var '&aux)
	       (setf now-processing :auxs))
	      ((listp var)
	       (case now-processing
		 (:required
		  (let ((sub-list-name (gensym "SUBLIST-")))
		    (push-sub-list-binding sub-list-name `(car ,path) var
					   name error-kind error-fun)
		    (parse-defmacro-lambda-list var sub-list-name name
						error-kind error-fun))
		  (setf path `(cdr ,path))
		  (incf minimum)
		  (incf maximum))
		 (:optionals
		  (when (> (length var) 3)
		    (cerror "Ignore extra noise."
			    "More than variable, initform, and suppliedp ~
			    in &optional binding - ~S"
			    var))
		  (push-optional-binding (car var) (cadr var) (caddr var)
					 `(not (null ,path)) `(car ,path)
					 name error-kind error-fun)
		  (setf path `(cdr ,path))
		  (incf maximum))
		 (:keywords
		  (let* ((keyword-given (consp (car var)))
			 (variable (if keyword-given
				       (cadar var)
				       (car var)))
			 (keyword (if keyword-given
				      (caar var)
				      (make-keyword variable)))
			 (supplied-p (caddr var)))
		    (push-optional-binding variable (cadr var) supplied-p
					   `(keyword-supplied-p ',keyword
								,rest-name)
					   `(lookup-keyword ',keyword
							    ,rest-name)
					   name error-kind error-fun)
		    (push keyword keys)))
		 (:auxs (push-let-binding (car var) (cadr var) nil))))
	      ((symbolp var)
	       (case now-processing
		 (:required
		  (incf minimum)
		  (incf maximum)
		  (push-let-binding var `(car ,path) nil)
		  (setf path `(cdr ,path)))
		 (:optionals
		  (incf maximum)
		  (push-let-binding var `(car ,path) nil `(not (null ,path)))
		  (setf path `(cdr ,path)))
		 (:keywords
		  (let ((key (make-keyword var)))
		    (push-let-binding var `(lookup-keyword ,key ,rest-name)
				      nil)
		    (push key keys)))
		 (:auxs
		  (push-let-binding var nil nil))))
	      (t
	       (error "Non-symbol in lambda-list - ~S." var)))))
    ;; Generate code to check the number of arguments, unless dotted
    ;; in which case length will not work.
    (unless restp
       (push `(unless (<= ,minimum
			  (length (the list ,(if top-level
						 `(cdr ,arg-list-name)
					       arg-list-name)))
			  ,@(unless restp
				    (list maximum)))
		      ,(let ((arg (if top-level
				      `(cdr ,arg-list-name)
				    arg-list-name)))
			 (if (eq error-fun 'error)
			     `(do-arg-count-error ',error-kind ',name ,arg
						  ',lambda-list ,minimum
						  ,(unless restp maximum))
			   `(,error-fun 'defmacro-ll-arg-count-error
				 :kind ',error-kind
				 ,@(when name `(:name ',name))
				 :argument ,arg
				 :lambda-list ',lambda-list
				 :minimum ,minimum
				 ,@(unless restp `(:maximum ,maximum))))))
	     *arg-tests*))
    (if keys
	(let ((problem (gensym "KEY-PROBLEM-"))
	      (info (gensym "INFO-")))
	  (push `(multiple-value-bind
		     (,problem ,info)
		     (verify-keywords ,rest-name ',keys ',allow-other-keys-p)
		   (when ,problem
		     (,error-fun
		      'defmacro-ll-broken-key-list-error
		      :kind ',error-kind
		      ,@(when name `(:name ',name))
		      :problem ,problem
		      :info ,info)))
		*arg-tests*)))
    (values env-arg-used minimum (if (null restp) maximum nil))))

;;; We save space in macro definitions by callig this function.
;;;
(defun do-arg-count-error (error-kind name arg lambda-list minimum maximum)
  (multiple-value-bind
      (fname debug:*stack-top-hint*)
      (kernel:find-caller-name)
    (error 'defmacro-ll-arg-count-error
	   :kind error-kind
	   :function-name fname
	   :name name
	   :argument arg
	   :lambda-list lambda-list
	   :minimum minimum :maximum maximum)))

(defun push-sub-list-binding (variable path object name error-kind error-fun)
  (let ((var (gensym "TEMP-")))
    (push `(,variable
	    (let ((,var ,path))
	      (if (listp ,var)
		  ,var
		  (,error-fun 'defmacro-bogus-sublist-error
			      :kind ',error-kind
			      ,@(when name `(:name ',name))
			      :object ,var
			      :lambda-list ',object))))
	  *system-lets*)))

(defun push-let-binding (variable path systemp &optional condition
				  (init-form *default-default*))
  (let ((let-form (if condition
		      `(,variable (if ,condition ,path ,init-form))
		      `(,variable ,path))))
    (if systemp
	(push let-form *system-lets*)
	(push let-form *user-lets*))))

(defun push-optional-binding (value-var init-form supplied-var condition path
					name error-kind error-fun)
  (unless supplied-var
    (setf supplied-var (gensym "SUPLIEDP-")))
  (push-let-binding supplied-var condition t)
  (cond ((consp value-var)
	 (let ((whole-thing (gensym "OPTIONAL-SUBLIST-")))
	   (push-sub-list-binding whole-thing
				  `(if ,supplied-var ,path ,init-form)
				  value-var name error-kind error-fun)
	   (parse-defmacro-lambda-list value-var whole-thing name
				       error-kind error-fun)))
	((symbolp value-var)
	 (push-let-binding value-var path nil supplied-var init-form))
	(t
	 (error "Illegal optional variable name: ~S" value-var))))

(defun make-keyword (symbol)
  "Takes a non-keyword symbol, symbol, and returns the corresponding keyword."
  (intern (symbol-name symbol) *keyword-package*))

(defun defmacro-error (problem kind name)
  (error "Illegal or ill-formed ~A argument in ~A~@[ ~S~]."
	 problem kind name))



;;;; Conditions signaled at runtime by the resultant body.

(define-condition defmacro-lambda-list-bind-error (error)
  ((kind :reader defmacro-lambda-list-bind-error-kind
	 :initarg :kind)
   (name :reader defmacro-lambda-list-bind-error-name
	 :initarg :name
	 :initform nil)))

(defun print-defmacro-ll-bind-error-intro (condition stream)
  (if (null (defmacro-lambda-list-bind-error-name condition))
      (format stream
	      "Error while parsing arguments to ~A in ~S:~%"
	      (defmacro-lambda-list-bind-error-kind condition)
	      (condition-function-name condition))
      (format stream
	      "Error while parsing arguments to ~A ~S:~%"
	      (defmacro-lambda-list-bind-error-kind condition)
	      (defmacro-lambda-list-bind-error-name condition))))

(define-condition defmacro-bogus-sublist-error
		  (defmacro-lambda-list-bind-error)
  ((object :reader defmacro-bogus-sublist-error-object :initarg :object)
   (lambda-list :reader defmacro-bogus-sublist-error-lambda-list
		:initarg :lambda-list))
  (:report
   (lambda (condition stream)
     (print-defmacro-ll-bind-error-intro condition stream)
     (format stream
	     "Bogus sublist:~%  ~S~%to satisfy lambda-list:~%  ~:S~%"
	     (defmacro-bogus-sublist-error-object condition)
	     (defmacro-bogus-sublist-error-lambda-list condition)))))

(define-condition defmacro-ll-arg-count-error (defmacro-lambda-list-bind-error)
  ((argument :reader defmacro-ll-arg-count-error-argument :initarg :argument)
   (lambda-list :reader defmacro-ll-arg-count-error-lambda-list
		:initarg :lambda-list)
   (minimum :reader defmacro-ll-arg-count-error-minimum :initarg :minimum)
   (maximum :reader defmacro-ll-arg-count-error-maximum :initarg :maximum))
  (:report
   (lambda (condition stream)
     (print-defmacro-ll-bind-error-intro condition stream)
     (format stream
	     "Invalid number of elements in:~%  ~:S~%~
	     to satisfy lambda-list:~%  ~:S~%"
	     (defmacro-ll-arg-count-error-argument condition)
	     (defmacro-ll-arg-count-error-lambda-list condition))
     (cond ((null (defmacro-ll-arg-count-error-maximum condition))
	    (format stream "Expected at least ~D"
		    (defmacro-ll-arg-count-error-minimum condition)))
	   ((= (defmacro-ll-arg-count-error-minimum condition)
	       (defmacro-ll-arg-count-error-maximum condition))
	    (format stream "Expected exactly ~D"
		    (defmacro-ll-arg-count-error-minimum condition)))
	   (t
	    (format stream "Expected between ~D and ~D"
		    (defmacro-ll-arg-count-error-minimum condition)
		    (defmacro-ll-arg-count-error-maximum condition))))
     (format stream ", but got ~D."
	     (length (defmacro-ll-arg-count-error-argument condition))))))


(define-condition defmacro-ll-broken-key-list-error
		  (defmacro-lambda-list-bind-error)
  ((problem :reader defmacro-ll-broken-key-list-error-problem
	    :initarg :problem)
   (info :reader defmacro-ll-broken-key-list-error-info :initarg :info))
  (:report (lambda (condition stream)
	     (print-defmacro-ll-bind-error-intro condition stream)
	     (format stream
		     (ecase
			 (defmacro-ll-broken-key-list-error-problem condition)
		       (:dotted-list
			"Keyword/value list is dotted: ~S")
		       (:odd-length
			"Odd number of elements in keyword/value list: ~S")
		       (:duplicate
			"Duplicate keyword: ~S")
		       (:unknown-keyword
			"~{Unknown keyword: ~S; expected one of ~{~S~^, ~}~}"))
		     (defmacro-ll-broken-key-list-error-info condition)))))
