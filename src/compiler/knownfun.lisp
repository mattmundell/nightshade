;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/knownfun.lisp,v 1.22 2000/07/07 09:33:03 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains stuff for maintaining a database of special
;;; information about functions known to the compiler.  This includes semantic
;;; information such as side-effects and type inference functions as well as
;;; transforms and IR2 translators.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package :c)

(export '(call unsafe unwind any foldable flushable movable predicate))

;;;; IR1 boolean function attributes:
;;;
;;;    There are a number of boolean attributes of known functions which we
;;; like to have in IR1.  This information is mostly side effect information of
;;; a sort, but it is different from the kind of information we want in IR2.
;;; We aren't interested in a fine breakdown of side effects, since we do very
;;; little code motion on IR1.  We are interested in some deeper semantic
;;; properties such as whether it is safe to pass stack closures to.
;;;
(def-boolean-attribute ir1
  ;;
  ;; May call functions that are passed as arguments.  In order to determine
  ;; what other effects are present, we must find the effects of all arguments
  ;; that may be functions.
  call
  ;;
  ;; May incorporate function or number arguments into the result or somehow
  ;; pass them upward.  Note that this applies to any argument that *might* be
  ;; a function or number, not just the arguments that always are.
  unsafe
  ;;
  ;; May fail to return during correct execution.  Errors are O.K.
  unwind
  ;;
  ;; The (default) worst case.  Includes all the other bad things, plus any
  ;; other possible bad thing.  If this is present, the above bad attributes
  ;; will be explicitly present as well.
  any
  ;;
  ;; May be constant-folded.  The function has no side effects, but may be
  ;; affected by side effects on the arguments.  e.g. SVREF, MAPC.  Functions
  ;; that side-effect their arguments are not considered to be foldable.
  ;; Although it would be "legal" to constant fold them (since it "is an error"
  ;; to modify a constant), we choose not to mark theses functions as foldable
  ;; in this database.
  foldable
  ;;
  ;; May be eliminated if value is unused.  The function has no side effects
  ;; except possibly CONS.  If a function is defined to signal errors, then it
  ;; is not flushable even if it is movable or foldable.
  flushable
  ;;
  ;; May be moved with impunity.  Has no side effects except possibly CONS, and
  ;; is affected only by its arguments.
  movable
  ;;
  ;; Function is a true predicate likely to be open-coded.  Convert any
  ;; non-conditional uses into (IF <pred> T NIL).
  predicate
  ;;
  ;; Inhibit any warning for compiling a recursive definition.  [Normally the
  ;; compiler warns when compiling a recursive definition for a known function,
  ;; since it might be a botched interpreter stub.]
  recursive
  ;;
  ;; Function does explicit argument type checking, so the declared type should
  ;; not be asserted when a definition is compiled.
  explicit-check)

(defstruct (function-info
	    (:print-function %print-function-info)
	    (:pure t))
  ;;
  ;; Boolean attributes of this function.
  (attributes (required-argument) :type attributes)
  ;;
  ;; A list of Transform structures describing transforms for this function.
  (transforms () :type list)
  ;;
  ;; A function which computes the derived type for a call to this function by
  ;; examining the arguments.  This is null when there is no special method for
  ;; this function.
  (derive-type nil :type (or function null))
  ;;
  ;; A function that does random unspecified code transformations by directly
  ;; hacking the IR.  Returns true if further optimizations of the call
  ;; shouldn't be attempted.
  (optimizer nil :type (or function null))
  ;;
  ;; If true, a special-case LTN annotation method that is used in place of the
  ;; standard type/policy template selection.  It may use arbitrary code to
  ;; choose a template, decide to do a full call, or conspire with the
  ;; IR2-Convert method to do almost anything.  The Combination node is passed
  ;; as the argument.
  (ltn-annotate nil :type (or function null))
  ;;
  ;; If true, the special-case IR2 conversion method for this function.  This
  ;; deals with funny functions, and anything else that can't be handled using
  ;; the template mechanism.  The Combination node and the IR2-Block are passed
  ;; as arguments.
  (ir2-convert nil :type (or function null))
  ;;
  ;; A list of all the templates that could be used to translate this function
  ;; into IR2, sorted by increasing cost.
  (templates nil :type list)
  ;;
  ;; If non-null, then this function is a unary type predicate for this type.
  (predicate-type nil :type (or ctype null))
  ;;
  ;; If non-null, use this function to annotate the known call for the byte
  ;; compiler.  If it returns NIL, then change the call to :full.
  (byte-annotate nil :type (or function null))
  ;;
  ;; If non-null, use this function to generate the byte code for this known
  ;; call.  This function can only give up if there is a byte-annotate function
  ;; that arranged for the functional to be pushed onto the stack.
  (byte-compile nil :type (or function null)))

(defprinter function-info
  (transforms :test transforms)
  (derive-type :test derive-type)
  (optimizer :test optimizer)
  (ltn-annotate :test ltn-annotate)
  (ir2-convert :test ir2-convert)
  (templates :test templates)
  (predicate-type :test predicate-type)
  (byte-annotate :test byte-annotate)
  (byte-compile :test byte-compile))


;;;; Interfaces to defining macros:

;;; The TRANSFORM structure represents an IR1 transform.
;;;
(defstruct (transform (:print-function %print-transform))
  ;;
  ;; The function-type which enables this transform.
  (type (required-argument) :type ctype)
  ;;
  ;; The transformation function.  Takes the Combination node and Returns a
  ;; lambda, or throws out.
  (function (required-argument) :type function)
  ;;
  ;; String used in efficency notes.
  (note (required-argument) :type string)
  ;;
  ;; T if we should spew a failure note even if speed=brevity.
  (important nil :type (member t nil))
  ;;
  ;; Usable for byte code, native code, or both.
  (when :native :type (member :byte :native :both)))

(defprinter transform type note important when)


;;; %Deftransform  --  Internal
;;;
;;;    Grab the Function-Info and enter the function, replacing any old one
;;; with the same type and note.
;;;
(defun %deftransform (name type fun &optional note important
			   (when :native))
  (declare (list type) (function fun)
	   (type (or string null) note)
	   (type (member t nil) important)
	   (type (member :native :byte :both) when))
  (let* ((ctype (specifier-type type))
	 (note (or note "optimize"))
	 (info (function-info-or-lose name))
	 (old (find-if #'(lambda (x)
			   (and (type= (transform-type x) ctype)
				(string-equal (transform-note x) note)
				(eq (transform-important x) important)
				(eq (transform-when x) when)))
		       (function-info-transforms info))))
    (if old
	(setf (transform-function old) fun (transform-note old) note)
	(push (make-transform :type ctype :function fun :note note
			      :important important :when when)
	      (function-info-transforms info)))
    name))


;;; %Defknown  --  Internal
;;;
;;;    Make a function-info structure with the specified type, attributes and
;;; optimizers.
;;;
(defun %defknown (names type attributes &key derive-type optimizer)
  (declare (list names type) (type attributes attributes)
	   (type (or function null) derive-type optimizer))
  (let ((ctype (specifier-type type))
	(info (make-function-info :attributes attributes
				  :derive-type derive-type
				  :optimizer optimizer))
	(target-env (or (backend-info-environment *target-backend*)
			*info-environment*)))
    (dolist (name names)
      (setf (info function type name target-env) ctype)
      (setf (info function where-from name target-env) :declared)
      (setf (info function kind name target-env) :function)
      (setf (info function info name target-env) info)))
  names)


;;; Function-Info-Or-Lose  --  Internal
;;;
;;;    Return the Function-Info for name or die trying.  Since this is used by
;;; people who want to modify the info, and the info may be shared, we copy it.
;;; We don't have to copy the lists, since each function that has generators or
;;; transforms has already been through here.
;;;
(defun function-info-or-lose (name)
  (declare (values function-info))
  (let ((*info-environment* (or (backend-info-environment *target-backend*)
				*info-environment*)))
    (let ((old (info function info name)))
      (unless old (error "~S is not a known function." name))
      (setf (info function info name) (copy-function-info old)))))


;;;; Generic type inference methods:

;;; RESULT-TYPE-xxx-ARG  --  Interface
;;;
;;;    Derive the type to be the type of the xxx'th arg.  This can normally
;;; only be done when the result value is that argument.
;;;
(defun result-type-first-arg (call)
  (declare (type combination call))
  (let ((cont (first (combination-args call))))
    (when cont (continuation-type cont))))
;;;
(defun result-type-last-arg (call)
  (declare (type combination call))
  (let ((cont (car (last (combination-args call)))))
    (when cont (continuation-type cont))))


;;; RESULT-TYPE-FLOAT-CONTAGION  --  Interface
;;;
;;;    Derive the result type according to the float contagion rules, but
;;; always return a float.  This is used for irrational functions that preserve
;;; realness of their arguments.
;;;
(defun result-type-float-contagion (call)
  (declare (type combination call))
  (reduce #'numeric-contagion (combination-args call)
	  :key #'continuation-type
	  :initial-value (specifier-type 'single-float)))


;;; SEQUENCE-RESULT-NTH-ARG  --  Internal
;;;
;;;    Return a closure usable as a derive-type method for accessing the N'th
;;; argument.  If arg is a list, result is a list.  If arg is a vector, result
;;; is a vector with the same element type.
;;;
(defun sequence-result-nth-arg (n)
  #'(lambda (call)
      (declare (type combination call))
      (let ((cont (nth (1- n) (combination-args call))))
	(when cont
	  (let ((type (continuation-type cont)))
	    (if (array-type-p type)
		(specifier-type
		 `(vector ,(type-specifier (array-type-element-type type))))
		(let ((ltype (specifier-type 'list)))
		  (when (csubtypep type ltype)
		    ltype))))))))


;;; RESULT-TYPE-SPECIFIER-NTH-ARG  --  Interface
;;;
;;;    Derive the type to be the type specifier which is the N'th arg.
;;; 
(defun result-type-specifier-nth-arg (n)
  #'(lambda (call)
      (declare (type combination call))
      (let ((cont (nth (1- n) (combination-args call))))
	(when (and cont (constant-continuation-p cont))
	  (specifier-type (continuation-value cont))))))
