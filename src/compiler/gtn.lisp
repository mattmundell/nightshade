;;; The Global TN Assignment pass in the compiler.  GTN allocates the TNs
;;; that hold the values of lexical variables and determines the calling
;;; conventions and passing locations used in function calls.

(in-package "C")

#[ Global TN Assignment

    Iterate over all defined functions, determining calling conventions and
    assigning Temporary Names (TNs) to local variables.

Phase position: 10/23 (middle)

Presence: required

Files: gtn

Entry functions: `gtn-analyze'

Call sequences:

    native-compile-component
      gtn-analyze
        make-ir2-component
        assign-ir2-environment
          make-normal-tn
          make-ir2-environment
        assign-return-locations
          make-ir2-nlx-info
        assign-lambda-var-tns
          environment-debug-live-tn


    XXX Rename this phase so as not to be confused with the local/global TN
        representation.

    The basic mechanism for closing over values is to pass the values as additional
    implicit arguments in the function call.  This technique is only applicable
    when:
     -- the calling function knows which values the called function wants to close
	over, and
     -- the values to be closed over are available in the calling environment.

    The first condition is always true of local function calls.  Environment
    analysis can guarantee that the second condition holds by closing over any
    needed values in the calling environment.

    If the function that closes over values may be called in an environment where
    the closed over values are not available, then we must store the values in a
    "closure" so that they are always accessible.  Closures are called using the
    "full call" convention.  When a closure is called, control is transferred to
    the "external entry point", which fetches the values out of the closure and
    then does a local call to the real function, passing the closure values as
    implicit arguments.

    In this scheme there is no such thing as a "heap closure variable" in code,
    since the closure values are moved into TNs by the external entry point.  There
    is some potential for pessimization here, since we may end up moving the values
    from the closure into a stack memory location, but the advantages are also
    substantial.  Simplicity is gained by always representing closure values the
    same way, and functions with closure references may still be called locally
    without allocating a closure.  All the TN based VMR optimizations will apply
    to closure variables, since closure variables are represented in the same way
    as all other variables in VMR.  Closure values will be allocated in registers
    where appropriate.

    Closures are created at the point where the function is referenced, eliminating
    the need to be able to close over closures.  This lazy creation of closures has
    the additional advantage that when a closure reference is conditionally not
    done, then the closure consing will never be done at all.  The corresponding
    disadvantage is that a closure over the same values may be created multiple
    times if there are multiple references.  Note however, that VMR loop and common
    subexpression optimizations can eliminate redundant closure consing.  In any
    case, multiple closures over the same variables doesn't seem to be that common.

	Having the Tail-Info would also make return convention determination trivial.
	We could just look at the type, checking to see if it represents a fixed number
	of values.  To determine if the standard return convention is necessary to
	preserve tail-recursion, we just iterate over the equivalent functions, looking
	for XEPs and uses in full calls.

FIX first mention of TN
        should be mentioned in [Virtual Machine Representation]

The Global Temporary Name Assignment pass (GTN) can be considered a
post-pass to environment analysis.  This phase assigns the TNs used to hold
local lexical variables and pass arguments and return values and determines
the value-passing strategy used in local calls.

To assign return locations, we look at the function's tail-set.

[Functions] describes the function call mechanism in the virtual machine.

If the result continuation for an entry point is used as the continuation for a
full call, then we may need to constrain the continuation's values passing
convention to the standard one.  This is not necessary when the call is known
not to be part of a tail-recursive loop (due to being a known function).

Once we have figured out where we must use the standard value passing strategy,
we can use a more flexible strategy to determine the return locations for local
functions.  We determine the possible numbers of return values from each
function by examining the uses of all the result continuations in the
equivalence class of the result continuation.

If the tail-set type is for a fixed number of
values, then we return that fixed number of values from all the functions whose
result continuations are equated.  If the number of values is not fixed, then
we must use the unknown-values convention, although we are not forced to use
the standard locations.  We assign the result TNs at this time.

We also use the tail-sets to see what convention we want to use.  What we do is
use the full convention for any function that has a XEP its tail-set, even if
we aren't required to do so by a tail-recursive full call, as long as there are
no non-tail-recursive local calls in the set.  This prevents us from
gratuitously using a non-standard convention when there is no reason to.
]#

;;; GTN-Analyze  --  Interface
;;;
;;; We make a pass over the component's environments, assigning argument
;;; passing locations and return conventions and TNs for local variables.
;;;
(defun gtn-analyze (component)
  (setf (component-info component) (make-ir2-component))
  (let ((funs (component-lambdas component)))
    (dolist (fun funs)
      (assign-ir2-environment fun)
      (assign-return-locations fun)
      (assign-ir2-nlx-info fun)
      (assign-lambda-var-tns fun nil)
      (dolist (let (lambda-lets fun))
	(assign-lambda-var-tns let t))))

  (undefined-value))

;;; Assign-Lambda-Var-TNs  --  Internal
;;;
;;; We have to allocate the home TNs for variables before we can call
;;; Assign-IR2-Environment so that we can close over TNs that haven't had         FIX called after a-i-e
;;; their home environment assigned yet.  Here we evaluate the
;;; DEBUG-INFO/SPEED tradeoff to determine how variables are allocated.  If
;;; SPEED is 3, then all variables are subject to lifetime analysis.
;;; Otherwise, only Let-P variables are allocated normally, and that can be
;;; inhibited by DEBUG-INFO = 3.
;;;
(defun assign-lambda-var-tns (fun let-p)
  (declare (type clambda fun))
  (dolist (var (lambda-vars fun))
    (when (leaf-refs var)
      (let* ((type (if (lambda-var-indirect var)
		       (backend-any-primitive-type *backend*)
		       (primitive-type (leaf-type var))))
	     (temp (make-normal-tn type))
	     (node (lambda-bind fun))
	     (res (if (or (and let-p (policy node (< debug 3)))
			  (policy node (zerop debug))
			  (policy node (= speed 3)))
		      temp
		      (environment-debug-live-tn temp
						 (lambda-environment fun)))))
	(setf (tn-leaf res) var)
	(setf (leaf-info var) res))))
  (undefined-value))

;;; Assign-IR2-Environment  --  Internal
;;;
;;; Give an IR2-Environment structure to Fun.  We make the TNs which hold
;;; environment values and the old-FP/return-PC.
;;;
(defun assign-ir2-environment (fun)
  (declare (type clambda fun))
  (let ((env (lambda-environment fun)))
    (collect ((env))
      (dolist (thing (environment-closure env))
	(let ((ptype (etypecase thing
		       (lambda-var
			(if (lambda-var-indirect thing)
			    (backend-any-primitive-type *backend*)
			    (primitive-type (leaf-type thing))))
		       (nlx-info (backend-any-primitive-type *backend*)))))
	  (env (cons thing (make-normal-tn ptype)))))

      (let ((res (make-ir2-environment
		  :environment (env)
		  :return-pc-pass (make-return-pc-passing-location
				   (external-entry-point-p fun)))))
	(setf (environment-info env) res)
	(setf (ir2-environment-old-fp res)
	      (make-old-fp-save-location env))
	(setf (ir2-environment-return-pc res)
	      (make-return-pc-save-location env)))))

  (undefined-value))

;;; Has-Full-Call-Use  --  Internal
;;;
;;; Return true if Fun's result continuation is used in a TR full call.  We
;;; only consider explicit :Full calls.  It is assumed that known calls are
;;; never part of a tail-recursive loop, so we don't need to enforce
;;; tail-recursion.  In any case, we don't know which known calls will
;;; actually be full calls until after LTN.
;;;
(defun has-full-call-use (fun)
  (declare (type clambda fun))
  (let ((return (lambda-return fun)))
    (and return
	 (do-uses (use (return-result return) nil)
	   (when (and (node-tail-p use)
		      (basic-combination-p use)
		      (eq (basic-combination-kind use) :full))
	     (return t))))))

;;; Use-Standard-Returns  --  Internal
;;;
;;; Return true if we should use the standard (unknown) return convention
;;; for a tail-set.  We use the standard return convention when:
;;; -- We must use the standard convention to preserve tail-recursion, since
;;;    the tail-set contains both an XEP and a TR full call.
;;; -- It appears to be more efficient to use the standard convention, since
;;;    there are no non-TR local calls that could benefit from a non-standard
;;;    convention.
;;;
(defun use-standard-returns (tails)
  (declare (type tail-set tails))
  (let ((funs (tail-set-functions tails)))
    (or (and (find-if #'external-entry-point-p funs)
	     (find-if #'has-full-call-use funs))
	(block punt
	  (dolist (fun funs t)
	    (dolist (ref (leaf-refs fun))
	      (let* ((cont (node-cont ref))
		     (dest (continuation-dest cont)))
		(when (and dest
			   (not (node-tail-p dest))
			   (basic-combination-p dest)
			   (eq (basic-combination-fun dest) cont)
			   (eq (basic-combination-kind dest) :local))
		  (return-from punt nil)))))))))

;;; RETURN-VALUE-EFFICENCY-NOTE  --  Internal
;;;
;;; If policy indicates, give an efficency note about our inability to use
;;; the known return convention.  We try to find a function in the tail set
;;; with non-constant return values to use as context.  If there is no such
;;; function, then be more vague.
;;;
(defun return-value-efficency-note (tails)
  (declare (type tail-set tails))
  (let ((funs (tail-set-functions tails)))
    (when (policy (lambda-bind (first funs)) (> (max speed space) brevity))
      (dolist (fun funs
		   (let ((*compiler-error-context* (lambda-bind (first funs))))
		     (compiler-note
		      "Return value count mismatch prevents known return ~
		       from these functions:~
		       ~{~%  ~A~}"
		      (remove nil (mapcar #'leaf-name funs)))))
	(let ((ret (lambda-return fun)))
	  (when ret
	    (let ((rtype (return-result-type ret)))
	      (multiple-value-bind (ignore count)
				   (values-types rtype)
		(declare (ignore ignore))
		(when (eq count :unknown)
		  (let ((*compiler-error-context* (lambda-bind fun)))
		    (compiler-note
		     "Return type not fixed values, so can't use known return ~
		      convention:~%  ~S"
		     (type-specifier rtype)))
		  (return)))))))))
  (undefined-value))

;;; Return-Info-For-Set  --  Internal
;;;
;;; Return a Return-Info structure describing how we should return from
;;; functions in the specified tail set.  We use the unknown values
;;; convention if the number of values is unknown, or if it is a good idea
;;; for some other reason.  Otherwise we allocate passing locations for a
;;; fixed number of values.
;;;
(defun return-info-for-set (tails)
  (declare (type tail-set tails))
  (multiple-value-bind (types count)
		       (values-types (tail-set-type tails))
    (let ((ptypes (mapcar #'primitive-type types))
	  (use-standard (use-standard-returns tails)))
      (when (and (eq count :unknown) (not use-standard))
	(return-value-efficency-note tails))
      (if (or (eq count :unknown) use-standard)
	  (make-return-info :kind :unknown  :count count  :types ptypes)
	  (make-return-info
	   :kind :fixed
	   :count count
	   :types ptypes
	   :locations (mapcar #'make-normal-tn ptypes))))))

;;; Assign-Return-Locations  --  Internal
;;;
;;; If Tail-Set doesn't have any Info, then make a Return-Info for it.  If
;;; we choose a return convention other than :Unknown, and this environment
;;; is for an XEP, then break tail recursion on the XEP calls, since we
;;; must always use unknown values when returning from an XEP.
;;;
(defun assign-return-locations (fun)
  (declare (type clambda fun))
  (let* ((tails (lambda-tail-set fun))
	 (returns (or (tail-set-info tails)
		      (setf (tail-set-info tails)
			    (return-info-for-set tails))))
	 (return (lambda-return fun)))
    (when (and return
	       (not (eq (return-info-kind returns) :unknown))
	       (external-entry-point-p fun))
      (do-uses (use (return-result return))
	(setf (node-tail-p use) nil))))
  (undefined-value))

;;; Assign-IR2-NLX-Info  --  Internal
;;;
;;; Make an IR2-NLX-Info structure for each NLX entry point recorded.  We
;;; call a VM supplied function to make the Save-SP restricted on the
;;; stack.  The NLX-Entry VOP's :Force-To-Stack Save-P value doesn't do
;;; this, since the SP is an argument to the VOP, and thus isn't live
;;; afterwards.
;;;
(defun assign-ir2-nlx-info (fun)
  (declare (type clambda fun))
  (let ((env (lambda-environment fun)))
    (dolist (nlx (environment-nlx-info env))
      (setf (nlx-info-info nlx)
	    (make-ir2-nlx-info
	     :home (when (member (cleanup-kind (nlx-info-cleanup nlx))
				 '(:block :tagbody))
		     (make-normal-tn (backend-any-primitive-type *backend*)))
	     :save-sp (make-nlx-sp-tn env)))))
  (undefined-value))
