;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/locall.lisp,v 1.51 2002/08/21 17:55:20 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file implements local call analysis.  A local call is a function
;;; call between functions being compiled at the same time.  If we can tell at
;;; compile time that such a call is legal, then we change the combination
;;; to call the correct lambda, mark it as local, and add this link to our call
;;; graph.  Once a call is local, it is then eligible for let conversion, which
;;; places the body of the function inline.
;;;
;;;    We cannot always do a local call even when we do have the function being
;;; called.  Calls that cannot be shown to have legal arg counts are not
;;; converted.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package :c)


;;; Propagate-To-Args  --  Interface
;;;
;;;    This function propagates information from the variables in the function
;;; Fun to the actual arguments in Call.  This is also called by the VALUES IR1
;;; optimizer when it sleazily converts MV-BINDs to LETs.
;;;
;;;    We flush all arguments to Call that correspond to unreferenced variables
;;; in Fun.  We leave NILs in the Combination-Args so that the remaining args
;;; still match up with their vars.
;;;
;;;    We also apply the declared variable type assertion to the argument
;;; continuations.
;;;
(defun propagate-to-args (call fun)
  (declare (type combination call) (type clambda fun)) 
  (do ((args (basic-combination-args call) (cdr args))
       (vars (lambda-vars fun) (cdr vars)))
      ((null args))
    (let ((arg (car args))
	  (var (car vars)))
      (cond ((leaf-refs var)
	     (assert-continuation-optional-type arg (leaf-type var)))
	    (t
	     (flush-dest arg)
	     (setf (car args) nil)))))
      
  (undefined-value))


;;; Merge-Tail-Sets  --  Interface
;;;
;;;    This function handles merging the tail sets if Call is potentially
;;; tail-recursive, and is a call to a function with a different TAIL-SET than
;;; Call's Fun.  This must be called whenever we alter IR1 so as to place a
;;; local call in what might be a TR context.  Note that any call which returns
;;; its value to a RETURN is considered potentially TR, since any implicit
;;; MV-PROG1 might be optimized away.
;;;
;;; We destructively modify the set for the calling function to represent both,
;;; and then change all the functions in callee's set to reference the first.
;;; If we do merge, we reoptimize the RETURN-RESULT continuation to cause
;;; IR1-OPTIMIZE-RETURN to recompute the tail set type.
;;;
(defun merge-tail-sets (call &optional (new-fun (combination-lambda call)))
  (declare (type basic-combination call) (type clambda new-fun))
  (let ((return (continuation-dest (node-cont call))))
    (when (return-p return)
      (let ((call-set (lambda-tail-set (node-home-lambda call)))
	    (fun-set (lambda-tail-set new-fun)))
	(unless (eq call-set fun-set)
	  (let ((funs (tail-set-functions fun-set)))
	    (dolist (fun funs)
	      (setf (lambda-tail-set fun) call-set))
	    (setf (tail-set-functions call-set)
		  (nconc (tail-set-functions call-set) funs)))
	  (reoptimize-continuation (return-result return))
	  t)))))


;;; Convert-Call  --  Internal
;;;
;;;    Convert a combination into a local call.  We PROPAGATE-TO-ARGS, set the
;;; combination kind to :Local, add Fun to the Calls of the function that the
;;; call is in, call MERGE-TAIL-SETS, then replace the function in the Ref node
;;; with the new function.
;;;
;;; We change the Ref last, since changing the reference can trigger let
;;; conversion of the new function, but will only do so if the call is local.
;;; Note that the replacement may trigger let conversion or other changes in
;;; IR1.  We must call MERGE-TAIL-SETS with NEW-FUN before the substitution,
;;; since after the substitution (and let conversion), the call may no longer
;;; be recognizable as tail-recursive.
;;;
(defun convert-call (ref call fun)
  (declare (type ref ref) (type combination call) (type clambda fun))
  (propagate-to-args call fun)
  (setf (basic-combination-kind call) :local)
  (pushnew fun (lambda-calls (node-home-lambda call)))
  (merge-tail-sets call fun)
  (change-ref-leaf ref fun)
  (undefined-value))


;;;; External entry point creation:

;;; Make-XEP-Lambda  --  Internal
;;;
;;;    Return a Lambda form that can be used as the definition of the XEP for
;;; Fun.
;;;
;;;    If Fun is a lambda, then we check the number of arguments (conditional
;;; on policy) and call Fun with all the arguments.
;;;
;;;    If Fun is an Optional-Dispatch, then we dispatch off of the number of
;;; supplied arguments by doing do an = test for each entry-point, calling the
;;; entry with the appropriate prefix of the passed arguments.
;;;
;;;    If there is a more arg, then there are a couple of optimizations that we
;;; make (more for space than anything else):
;;; -- If Min-Args is 0, then we make the more entry a T clause, since no
;;;    argument count error is possible.
;;; -- We can omit the = clause for the last entry-point, allowing the case of
;;;    0 more args to fall through to the more entry.
;;;
;;;    We don't bother to policy conditionalize wrong arg errors in optional
;;; dispatches, since the additional overhead is negligible compared to the
;;; other hair going down.
;;;
;;;    Note that if policy indicates it, argument type declarations in Fun will
;;; be verified.  Since nothing is known about the type of the XEP arg vars,
;;; type checks will be emitted when the XEP's arg vars are passed to the
;;; actual function.
;;;
(defun make-xep-lambda (fun)
  (declare (type functional fun))
  (etypecase fun
    (clambda
     (let ((nargs (length (lambda-vars fun)))
	   (n-supplied (gensym)))
       (collect ((temps))
	 (dotimes (i nargs)
	   (temps (gensym)))
	 `(lambda (,n-supplied ,@(temps))
	    (declare (type index ,n-supplied))
	    ,(if (policy nil (zerop safety))
		 `(declare (ignore ,n-supplied))
		 `(%verify-argument-count ,n-supplied ,nargs))
	    (%funcall ,fun ,@(temps))))))
    (optional-dispatch
     (let* ((min (optional-dispatch-min-args fun))
	    (max (optional-dispatch-max-args fun))
	    (more (optional-dispatch-more-entry fun))
	    (n-supplied (gensym)))
       (collect ((temps)
		 (entries))
	 (dotimes (i max)
	   (temps (gensym)))

	 (do ((eps (optional-dispatch-entry-points fun) (rest eps))
	      (n min (1+ n)))
	     ((null eps))
	   (entries `((= ,n-supplied ,n)
		      (%funcall ,(first eps) ,@(subseq (temps) 0 n)))))

	 `(lambda (,n-supplied ,@(temps))
	    (declare (type index ,n-supplied))
	    (cond
	     ,@(if more (butlast (entries)) (entries))
	     ,@(when more
		 `((,(if (zerop min) 't `(>= ,n-supplied ,max))
		    ,(let ((n-context (gensym))
			   (n-count (gensym)))
		       `(multiple-value-bind
			    (,n-context ,n-count)
			    (%more-arg-context ,n-supplied ,max)
			  (%funcall ,more ,@(temps) ,n-context ,n-count))))))
	     (t
	      (%argument-count-error ,n-supplied)))))))))


;;; Make-External-Entry-Point  --  Internal
;;;
;;;    Make an external entry point (XEP) for Fun and return it.  We convert
;;; the result of Make-XEP-Lambda in the correct environment, then associate
;;; this lambda with Fun as its XEP.  After the conversion, we iterate over the
;;; function's associated lambdas, redoing local call analysis so that the XEP
;;; calls will get converted.  We also bind *lexical-environment* to change the
;;; compilation policy over to the interface policy.
;;;
;;;    We set Reanalyze and Reoptimize in the component, just in case we
;;; discover an XEP after the initial local call analyze pass.
;;;
(defun make-external-entry-point (fun)
  (declare (type functional fun))
  (assert (not (functional-entry-function fun)))
  (with-ir1-environment (lambda-bind (main-entry fun))
    (let* ((*lexical-environment*
	    (make-lexenv :cookie
			 (make-interface-cookie *lexical-environment*)))
	   (res (ir1-convert-lambda (make-xep-lambda fun))))
      (setf (functional-kind res) :external)
      (setf (leaf-ever-used res) t)
      (setf (functional-entry-function res) fun)
      (setf (functional-entry-function fun) res)
      (setf (component-reanalyze *current-component*) t)
      (setf (component-reoptimize *current-component*) t)
      (etypecase fun
	(clambda (local-call-analyze-1 fun))
	(optional-dispatch
	 (dolist (ep (optional-dispatch-entry-points fun))
	   (local-call-analyze-1 ep))
	 (when (optional-dispatch-more-entry fun)
	   (local-call-analyze-1 (optional-dispatch-more-entry fun)))))
      res)))


;;; Reference-Entry-Point  --  Internal
;;;
;;;    Notice a Ref that is not in a local-call context.  If the Ref is already
;;; to an XEP, then do nothing, otherwise change it to the XEP, making an XEP
;;; if necessary.
;;;
;;;    If Ref is to a special :Cleanup or :Escape function, then we treat it as
;;; though it was not an XEP reference (i.e. leave it alone.)
;;;
(defun reference-entry-point (ref)
  (declare (type ref ref))
  (let ((fun (ref-leaf ref)))
    (unless (or (external-entry-point-p fun)
		(member (functional-kind fun) '(:escape :cleanup)))
      (change-ref-leaf ref (or (functional-entry-function fun)
			       (make-external-entry-point fun))))))



;;; Local-Call-Analyze-1  --  Interface
;;;
;;;    Attempt to convert all references to Fun to local calls.  The reference
;;; must be the function for a call, and the function continuation must be used
;;; only once, since otherwise we cannot be sure what function is to be called.
;;; The call continuation would be multiply used if there is hairy stuff such
;;; as conditionals in the expression that computes the function.
;;;
;;;    If we cannot convert a reference, then we mark the referenced function
;;; as an entry-point, creating a new XEP if necessary.  We don't try to
;;; convert calls that are in error (:ERROR kind.)
;;;
;;;    This is broken off from Local-Call-Analyze so that people can force
;;; analysis of newly introduced calls.  Note that we don't do let conversion
;;; here.
;;;
(defun local-call-analyze-1 (fun)
  (declare (type functional fun))
  (let ((refs (leaf-refs fun))
	(first-time t))
    (dolist (ref refs)
      (let* ((cont (node-cont ref))
	     (dest (continuation-dest cont)))
	(cond ((and (basic-combination-p dest)
		    (eq (basic-combination-fun dest) cont)
		    (eq (continuation-use cont) ref))

	       (convert-call-if-possible ref dest)
	       
	       (unless (eq (basic-combination-kind dest) :local)
		 (reference-entry-point ref)))
	      (t
	       (reference-entry-point ref))))
      (setq first-time nil)))

  (undefined-value))


;;; Local-Call-Analyze  --  Interface
;;;
;;;    We examine all New-Functions in component, attempting to convert calls
;;; into local calls when it is legal.  We also attempt to convert each lambda
;;; to a let.  Let conversion is also triggered by deletion of a function
;;; reference, but functions that start out eligible for conversion must be
;;; noticed sometime.
;;;
;;;    Note that there is a lot of action going on behind the scenes here,
;;; triggered by reference deletion.  In particular, the Component-Lambdas are
;;; being hacked to remove newly deleted and let converted lambdas, so it is
;;; important that the lambda is added to the Component-Lambdas when it is.
;;; Also, the COMPONENT-NEW-FUNCTIONS may contain all sorts of drivel, since it
;;; is not updated when we delete functions, etc.  Only COMPONENT-LAMBDAS is
;;; updated.
;;;
;;; COMPONENT-REANALYZE-FUNCTIONS is treated similarly to NEW-FUNCTIONS, but we
;;; don't add lambdas to the LAMBDAS.
;;;
(defun local-call-analyze (component)
  (declare (type component component))
  (loop
    (let* ((new (pop (component-new-functions component)))
	   (fun (or new (pop (component-reanalyze-functions component)))))
      (unless fun (return))
      (let ((kind (functional-kind fun)))
	(cond ((member kind '(:deleted :let :mv-let :assignment)))
	      ((and (null (leaf-refs fun)) (eq kind nil)
		    (not (functional-entry-function fun)))
	       (delete-functional fun))
	      (t
	       (when (and new (lambda-p fun))
		 (push fun (component-lambdas component)))
	       (local-call-analyze-1 fun)
	       (when (lambda-p fun)
		 (maybe-let-convert fun)))))))
  
  (undefined-value))


;;; MAYBE-EXPAND-LOCAL-INLINE  --  Internal
;;;
;;;    If policy is auspicious, Call is not in an XEP, and we don't seem to be
;;; in an infinite recursive loop, then change the reference to reference a
;;; fresh copy.  We return whichever function we decide to reference.
;;;
(defun maybe-expand-local-inline (fun ref call)
  (if (and (policy call (>= speed space) (>= speed cspeed))
	   (not (eq (functional-kind (node-home-lambda call)) :external))
	   (not *converting-for-interpreter*)
	   (inline-expansion-ok call))
      (with-ir1-environment call
	(let* ((*lexical-environment* (functional-lexenv fun))
	       (won nil)
	       (res (catch 'local-call-lossage
		      (prog1
			  (ir1-convert-lambda (functional-inline-expansion fun))
			(setq won t)))))
	  (cond (won
		 (change-ref-leaf ref res)
		 res)
		(t
		 (let ((*compiler-error-context* call))
		   (compiler-note "Couldn't inline expand because expansion ~
				   calls this let-converted local function:~
				   ~%  ~S"
				  (leaf-name res)))
		 fun))))
      fun))


;;; Convert-Call-If-Possible  --  Interface
;;;
;;;    Dispatch to the appropriate function to attempt to convert a call.  Ref
;;; most be a reference to a FUNCTIONAL.  This is called in IR1 optimize as
;;; well as in local call analysis.  If the call is is already :Local, we do
;;; nothing.  If the call is already scheduled for deletion, also do nothing
;;; (in addition to saving time, this also avoids some problems with optimizing
;;; collections of functions that are partially deleted.)
;;;
;;;    This is called both before and after FIND-INITIAL-DFO runs.  When called
;;; on a :INITIAL component, we don't care whether the caller and callee are in
;;; the same component.  Afterward, we must stick with whatever component
;;; division we have chosen.
;;;
;;;    Before attempting to convert a call, we see if the function is supposed
;;; to be inline expanded.  Call conversion proceeds as before after any
;;; expansion.
;;;
;;;    We bind *Compiler-Error-Context* to the node for the call so that
;;; warnings will get the right context.
;;;
;;;
(defun convert-call-if-possible (ref call)
  (declare (type ref ref) (type basic-combination call))
  (let* ((block (node-block call))
	 (component (block-component block))
	 (original-fun (ref-leaf ref)))
    (assert (functional-p original-fun))
    (unless (or (member (basic-combination-kind call) '(:local :error))
		(block-delete-p block)
		(eq (functional-kind (block-home-lambda block)) :deleted)
		(member (functional-kind original-fun)
			'(:top-level-xep :deleted))
		(not (or (eq (component-kind component) :initial)
			 (eq (block-component
			      (node-block
			       (lambda-bind (main-entry original-fun))))
			     component))))
      (let ((fun (if (external-entry-point-p original-fun)
		     (functional-entry-function original-fun)
		     original-fun))
	    (*compiler-error-context* call))
	
	(when (and (eq (functional-inlinep fun) :inline)
		   (rest (leaf-refs original-fun)))
	  (setq fun (maybe-expand-local-inline fun ref call)))
	
	(assert (member (functional-kind fun)
			'(nil :escape :cleanup :optional)))
	(cond ((mv-combination-p call)
	       (convert-mv-call ref call fun))
	      ((lambda-p fun)
	       (convert-lambda-call ref call fun))
	      (t
	       (convert-hairy-call ref call fun))))))

  (undefined-value))


;;; Convert-MV-Call  --  Internal
;;;
;;;    Attempt to convert a multiple-value call.  The only interesting case is
;;; a call to a function that Looks-Like-An-MV-Bind, has exactly one reference
;;; and no XEP, and is called with one values continuation.
;;;
;;;    We change the call to be to the last optional entry point and change the
;;; call to be local.  Due to our preconditions, the call should eventually be
;;; converted to a let, but we can't do that now, since there may be stray
;;; references to the e-p lambda due to optional defaulting code.
;;;
;;;    We also use variable types for the called function to construct an
;;; assertion for the values continuation.
;;;
;;; See CONVERT-CALL for additional notes on MERGE-TAIL-SETS, etc.
;;;
(defun convert-mv-call (ref call fun)
  (declare (type ref ref) (type mv-combination call) (type functional fun))
  (when (and (looks-like-an-mv-bind fun)
	     (not (functional-entry-function fun))
	     (= (length (leaf-refs fun)) 1)
	     (= (length (basic-combination-args call)) 1))
    (let ((ep (car (last (optional-dispatch-entry-points fun)))))
      (setf (basic-combination-kind call) :local)
      (pushnew ep (lambda-calls (node-home-lambda call)))
      (merge-tail-sets call ep)
      (change-ref-leaf ref ep)
      
      (assert-continuation-type
       (first (basic-combination-args call))
       (make-values-type :optional (mapcar #'leaf-type (lambda-vars ep))
			 :rest *universal-type*))))
  (undefined-value))


;;; Convert-Lambda-Call  --  Internal
;;;
;;;    Attempt to convert a call to a lambda.  If the number of args is wrong,
;;; we give a warning and mark the call as :ERROR to remove it from future
;;; consideration.  If the argcount is O.K. then we just convert it.
;;;
(defun convert-lambda-call (ref call fun)
  (declare (type ref ref) (type combination call) (type clambda fun))
  (let ((nargs (length (lambda-vars fun)))
	(call-args (length (combination-args call))))
    (cond ((= call-args nargs)
	   (convert-call ref call fun))
	  (t
	   (compiler-warning
	    "Function called with ~R argument~:P, but wants exactly ~R."
	    call-args nargs)
	   (setf (basic-combination-kind call) :error)))))



;;;; Optional, more and keyword calls:

;;; Convert-Hairy-Call  --  Internal
;;;
;;;    Similar to Convert-Lambda-Call, but deals with Optional-Dispatches.  If
;;; only fixed args are supplied, then convert a call to the correct entry
;;; point.  If keyword args are supplied, then dispatch to a subfunction.  We
;;; don't convert calls to functions that have a more (or rest) arg.
;;;
(defun convert-hairy-call (ref call fun)
  (declare (type ref ref) (type combination call)
	   (type optional-dispatch fun))
  (let ((min-args (optional-dispatch-min-args fun))
	(max-args (optional-dispatch-max-args fun))
	(call-args (length (combination-args call))))
    (cond ((< call-args min-args)
	   (compiler-warning "Function called with ~R argument~:P, but wants at least ~R."
			     call-args min-args)
	   (setf (basic-combination-kind call) :error))
	  ((<= call-args max-args)
	   (convert-call ref call
			 (elt (optional-dispatch-entry-points fun)
			      (- call-args min-args))))
	  ((optional-dispatch-more-entry fun)
	   (convert-more-call ref call fun))
	  (t
	   (compiler-warning "Function called with ~R argument~:P, but wants at most ~R."
			     call-args max-args)
	   (setf (basic-combination-kind call) :error))))
  (undefined-value))


;;; Convert-Hairy-Fun-Entry  --  Internal
;;;
;;;     This function is used to convert a call to an entry point when complex
;;; transformations need to be done on the original arguments.  Entry is the
;;; entry point function that we are calling.  Vars is a list of variable names
;;; which are bound to the oringinal call arguments.  Ignores is the subset of
;;; Vars which are ignored.  Args is the list of arguments to the entry point
;;; function.
;;;
;;;    In order to avoid gruesome graph grovelling, we introduce a new function
;;; that rearranges the arguments and calls the entry point.  We analyze the
;;; new function and the entry point immediately so that everything gets
;;; converted during the single pass.
;;;
(defun convert-hairy-fun-entry (ref call entry vars ignores args)
  (declare (list vars ignores args) (type ref ref) (type combination call)
	   (type clambda entry))
  (let ((new-fun
	 (with-ir1-environment call
	   (ir1-convert-lambda
	    `(lambda ,vars
	       (declare (ignorable . ,ignores))
	       (%funcall ,entry . ,args))))))
    (convert-call ref call new-fun)
    (dolist (ref (leaf-refs entry))
      (convert-call-if-possible ref (continuation-dest (node-cont ref))))))


;;; Convert-More-Call  --  Internal
;;;
;;;    Use Convert-Hairy-Fun-Entry to convert a more-arg call to a known
;;; function into a local call to the Main-Entry.
;;;
;;;    First we verify that all keywords are constant and legal.  If there
;;; aren't, then we warn the user and don't attempt to convert the call.
;;;
;;;    We massage the supplied keyword arguments into the order expected by the
;;; main entry.  This is done by binding all the arguments to the keyword call
;;; to variables in the introduced lambda, then passing these values variables
;;; in the correct order when calling the main entry.  Unused arguments
;;; (such as the keywords themselves) are discarded simply by not passing them
;;; along.
;;;
;;;    If there is a rest arg, then we bundle up the args and pass them to
;;; LIST.
;;;
(defun convert-more-call (ref call fun)
  (declare (type ref ref) (type combination call) (type optional-dispatch fun))
  (let* ((max (optional-dispatch-max-args fun))
	 (arglist (optional-dispatch-arglist fun))
	 (args (combination-args call))
	 (more (nthcdr max args))
	 (flame (policy call (or (> speed brevity) (> space brevity))))
	 (loser nil))
    (collect ((temps)
	      (more-temps)
	      (ignores)
	      (supplied)
	      (key-vars))

      (dolist (var arglist)
	(let ((info (lambda-var-arg-info var)))
	  (when info
	    (ecase (arg-info-kind info)
	      (:keyword
	       (key-vars var))
	      ((:rest :optional))
	      ((:more-context :more-count)
	       (compiler-warning "Can't local-call functions with &MORE args.")
	       (setf (basic-combination-kind call) :error)
	       (return-from convert-more-call))))))

      (dotimes (i max)
	(temps (gensym "FIXED-ARG-TEMP-")))

      (dotimes (i (length more))
	(more-temps (gensym "MORE-ARG-TEMP-")))

      (when (optional-dispatch-keyp fun)
	(when (oddp (length more))
	  (compiler-warning "Function called with odd number of ~
	  		     arguments in keyword portion.")

	  (setf (basic-combination-kind call) :error)
	  (return-from convert-more-call))

	(do ((key more (cddr key))
	     (temp (more-temps) (cddr temp)))
	    ((null key))
	  (let ((cont (first key)))
	    (unless (constant-continuation-p cont)
	      (when flame
		(compiler-note "Non-constant keyword in keyword call."))
	      (setf (basic-combination-kind call) :error)
	      (return-from convert-more-call))
	    
	    (let ((name (continuation-value cont))
		  (dummy (first temp))
		  (val (second temp)))
	      (dolist (var (key-vars)
			   (progn
			     (ignores dummy val)
			     (setq loser name)))
		(let ((info (lambda-var-arg-info var)))
		  (when (eq (arg-info-keyword info) name)
		    (ignores dummy)
		    (supplied (cons var val))
		    (return)))))))
	
	(when (and loser (not (optional-dispatch-allowp fun)))
	  (compiler-warning "Function called with unknown argument keyword ~S."
			    loser)
	  (setf (basic-combination-kind call) :error)
	  (return-from convert-more-call)))

      (collect ((call-args))
	(do ((var arglist (cdr var))
	     (temp (temps) (cdr temp)))
	    (())
	  (let ((info (lambda-var-arg-info (car var))))
	    (if info
		(ecase (arg-info-kind info)
		  (:optional
		   (call-args (car temp))
		   (when (arg-info-supplied-p info)
		     (call-args t)))
		  (:rest
		   (call-args `(list ,@(more-temps)))
		   (return))
		  (:keyword
		   (return)))
		(call-args (car temp)))))

	(dolist (var (key-vars))
	  (let ((info (lambda-var-arg-info var))
		(temp (cdr (assoc var (supplied)))))
	    (if temp
		(call-args temp)
		(call-args (arg-info-default info)))
	    (when (arg-info-supplied-p info)
	      (call-args (not (null temp))))))

	(convert-hairy-fun-entry ref call (optional-dispatch-main-entry fun)
				 (append (temps) (more-temps))
				 (ignores) (call-args)))))

  (undefined-value))


;;;; Let conversion:
;;;
;;;    Converting to a let has differing significance to various parts of the
;;; compiler:
;;; -- The body of a Let is spliced in immediately after the corresponding
;;;    combination node, making the control transfer explicit and allowing lets
;;;    to mashed together into a single block.  The value of the let is
;;;    delivered directly to the original continuation for the call,
;;;    eliminating the need to propagate information from the dummy result
;;;    continuation. 
;;; -- As far as IR1 optimization is concerned, it is interesting in that there
;;;    is only one expression that the variable can be bound to, and this is
;;;    easily substitited for.
;;; -- Lets are interesting to environment analysis and the back end because in
;;;    most ways a let can be considered to be "the same function" as its home
;;;    function.
;;; -- Let conversion has dynamic scope implications, since control transfers
;;;    within the same environment are local.  In a local control transfer,
;;;    cleanup code must be emitted to remove dynamic bindings that are no
;;;    longer in effect.


;;; Insert-Let-Body  --  Internal
;;;
;;;    Set up the control transfer to the called lambda.  We split the call
;;; block immediately after the call, and link the head of Fun to the call
;;; block.  The successor block after splitting (where we return to) is
;;; returned.
;;;
;;;    If the lambda is is a different component than the call, then we call
;;; JOIN-COMPONENTS.  This only happens in block compilation before
;;; FIND-INITIAL-DFO.
;;;
(defun insert-let-body (fun call)
  (declare (type clambda fun) (type basic-combination call))
  (let* ((call-block (node-block call))
	 (bind-block (node-block (lambda-bind fun)))
	 (component (block-component call-block)))
    (let ((fun-component (block-component bind-block)))
      (unless (eq fun-component component)
	(assert (eq (component-kind component) :initial))
	(join-components component fun-component)))

    (let ((*current-component* component))
      (node-ends-block call))
    (assert (= (length (block-succ call-block)) 1))
    (let ((next-block (first (block-succ call-block))))
      (unlink-blocks call-block next-block)
      (link-blocks call-block bind-block)
      next-block)))


;;; Merge-Lets  --  Internal
;;;
;;;    Handle the environment semantics of let conversion.  We add the lambda
;;; and its lets to lets for the Call's home function.  We merge the calls for
;;; Fun with the calls for the home function, removing Fun in the process.  We
;;; also merge the Entries.
;;;
;;;   We also unlink the function head from the component head and set
;;; Component-Reanalyze to true to indicate that the DFO should be recomputed.
;;;
(defun merge-lets (fun call)
  (declare (type clambda fun) (type basic-combination call))
  (let ((component (block-component (node-block call))))
    (unlink-blocks (component-head component) (node-block (lambda-bind fun)))
    (setf (component-lambdas component)
	  (delete fun (component-lambdas component)))
    (setf (component-reanalyze component) t))
  (setf (lambda-call-lexenv fun) (node-lexenv call))
  (let ((tails (lambda-tail-set fun)))
    (setf (tail-set-functions tails)
	  (delete fun (tail-set-functions tails))))
  (setf (lambda-tail-set fun) nil)
  (let* ((home (node-home-lambda call))
	 (home-env (lambda-environment home)))

    (assert (not (eq home fun)))

    ;; FUN belongs to HOME now.
    (push fun (lambda-lets home))
    (setf (lambda-home fun) home)
    (setf (lambda-environment fun) home-env)

    ;; All of FUN's LETs belong to HOME now
    (let ((lets (lambda-lets fun)))
      (dolist (let lets)
	(setf (lambda-home let) home)
	(setf (lambda-environment let) home-env))

      (setf (lambda-lets home) (nconc lets (lambda-lets home)))
      ;; FUN no longer has an independent existence as an entity which
      ;; has LETs.
      (setf (lambda-lets fun) ()))

    ;; HOME no longer calls FUN, and owns all of FUN's old DFO
    ;; dependencies
    (setf (lambda-calls home)
	  (delete fun (nunion (lambda-calls fun) (lambda-calls home))))
    ;; FUN no longer has an independent existence as an entity
    ;; which calls things or has DFO dependencies.
    (setf (lambda-calls fun) ())

    ;; All of FUN's ENTRIES belong to HOME now.
    (setf (lambda-entries home)
	  (nconc (lambda-entries fun) (lambda-entries home)))
    ;; FUN no longer has an independent existence as an entity
    ;; with ENTRIES.
    (setf (lambda-entries fun) ()))
  (undefined-value))


;;; Move-Return-Uses  --  Internal
;;;
;;;    Handle the value semantics of let conversion.  Delete Fun's return node,
;;; and change the control flow to transfer to Next-Block instead.  Move all
;;; the uses of the result continuation to Call's Cont.
;;;
;;;    If the actual continuation is only used by the let call, then we
;;; intersect the type assertion on the dummy continuation with the assertion
;;; for the actual continuation; in all other cases assertions on the dummy
;;; continuation are lost.
;;;
;;;    We also intersect the derived type of the call with the derived type of
;;; all the dummy continuation's uses.  This serves mainly to propagate
;;; TRULY-THE through lets.
;;;
(defun move-return-uses (fun call next-block)
  (declare (type clambda fun) (type basic-combination call)
	   (type cblock next-block))
  (let* ((return (lambda-return fun))
	 (return-block (node-block return)))
    (unlink-blocks return-block
		   (component-tail (block-component return-block)))
    (link-blocks return-block next-block)
    (unlink-node return)
    (delete-return return)
    (let ((result (return-result return))
	  (cont (node-cont call))
	  (call-type (node-derived-type call)))
      (when (eq (continuation-use cont) call)
	(assert-continuation-type cont (continuation-asserted-type result)))
      (unless (eq call-type *wild-type*)
	(do-uses (use result)
	  (derive-node-type use call-type)))
      (substitute-continuation-uses cont result)))
  (undefined-value))



;;; MOVE-LET-CALL-CONT  --  Internal
;;;
;;;    Change all Cont for all the calls to Fun to be the start continuation
;;; for the bind node.  This allows the blocks to be joined if the caller count
;;; ever goes to one.
;;;
(defun move-let-call-cont (fun)
  (declare (type clambda fun))
  (let ((new-cont (node-prev (lambda-bind fun))))
    (dolist (ref (leaf-refs fun))
      (let ((dest (continuation-dest (node-cont ref))))
	(delete-continuation-use dest)
	(add-continuation-use dest new-cont))))
  (undefined-value))


;;; Unconvert-Tail-Calls  --  Internal
;;;
;;;    We are converting Fun to be a let when the call is in a non-tail
;;; position.  Any previously tail calls in Fun are no longer tail calls, and
;;; must be restored to normal calls which transfer to Next-Block (Fun's
;;; return point.)  We can't do this by DO-USES on the RETURN-RESULT, because
;;; the return might have been deleted (if all calls were TR.)
;;;
;;;    The called function might be an assignment in the case where we are
;;; currently converting that function.  In steady-state, assignments never
;;; appear in the lambda-calls.
;;;
(defun unconvert-tail-calls (fun call next-block)
  (dolist (called (lambda-calls fun))
    (dolist (ref (leaf-refs called))
      (let ((this-call (continuation-dest (node-cont ref))))
	(when (and (node-tail-p this-call)
		   (eq (node-home-lambda this-call) fun))
	  (setf (node-tail-p this-call) nil)
	  (ecase (functional-kind called)
	    ((nil :cleanup :optional)
	     (let ((block (node-block this-call))
		   (cont (node-cont call)))
	       (ensure-block-start cont)
	       (unlink-blocks block (first (block-succ block)))
	       (link-blocks block next-block)
	       (delete-continuation-use this-call)
	       (add-continuation-use this-call cont)))
	    (:deleted)
	    (:assignment
	     (assert (eq called fun))))))))
  (undefined-value))


;;; MOVE-RETURN-STUFF  --  Internal
;;;
;;;    Deal with returning from a let or assignment that we are converting.
;;; FUN is the function we are calling, CALL is a call to FUN, and NEXT-BLOCK
;;; is the return point for a non-tail call, or NULL if call is a tail call.
;;;
;;; If the call is not a tail call, then we must do UNCONVERT-TAIL-CALLS, since
;;; a tail call is a call which returns its value out of the enclosing non-let
;;; function.  When call is non-TR, we must convert it back to an ordinary
;;; local call, since the value must be delivered to the receiver of CALL's
;;; value.
;;;
;;; We do different things depending on whether the caller and callee have
;;; returns left:
;;; -- If the callee has no return we just do MOVE-LET-CALL-CONT.  Either the
;;;    function doesn't return, or all returns are via tail-recursive local
;;;    calls.
;;; -- If CALL is a non-tail call, or if both have returns, then we
;;;    delete the callee's return, move its uses to the call's result
;;;    continuation, and transfer control to the appropriate return point.
;;; -- If the callee has a return, but the caller doesn't, then we move the
;;;    return to the caller.
;;;
(defun move-return-stuff (fun call next-block)
  (declare (type clambda fun) (type basic-combination call)
	   (type (or cblock null) next-block))
  (when next-block
    (unconvert-tail-calls fun call next-block))
  (let* ((return (lambda-return fun))
	 (call-fun (node-home-lambda call))
	 (call-return (lambda-return call-fun)))
    (cond ((not return))
	  ((or next-block call-return)
	   (unless (block-delete-p (node-block return))
	     (move-return-uses fun call
			       (or next-block (node-block call-return)))))
	  (t
	   (assert (node-tail-p call))
	   (setf (lambda-return call-fun) return)
	   (setf (return-lambda return) call-fun))))
  (move-let-call-cont fun)
  (undefined-value))


;;; Let-Convert  --  Internal
;;;
;;;    Actually do let conversion.  We call subfunctions to do most of the
;;; work.  We change the Call's cont to be the continuation heading the bind
;;; block, and also do Reoptimize-Continuation on the args and Cont so that
;;; let-specific IR1 optimizations get a chance.  We blow away any entry for
;;; the function in *free-functions* so that nobody will create new reference
;;; to it.
;;;
(defun let-convert (fun call)
  (declare (type clambda fun) (type basic-combination call))
  (let ((next-block (if (node-tail-p call)
			nil
			(insert-let-body fun call))))
    (move-return-stuff fun call next-block)
    (merge-lets fun call)))


;;; REOPTIMIZE-CALL  --  Internal
;;;
;;;    Reoptimize all of Call's args and its result.
;;;
(defun reoptimize-call (call)
  (declare (type basic-combination call))
  (dolist (arg (basic-combination-args call))
    (when arg
      (reoptimize-continuation arg)))
  (reoptimize-continuation (node-cont call))
  (undefined-value))

;;;  OK-INITIAL-CONVERT-P  --  Internal
;;;
;;; We also don't convert calls to named functions which appear in the initial
;;; component, delaying this until optimization.  This minimizes the likelyhood
;;; that we well let-convert a function which may have references added due to
;;; later local inline expansion
;;;
(defun ok-initial-convert-p (fun)
  (not (and (leaf-name fun)
	    (eq (component-kind
		 (block-component
		  (node-block (lambda-bind fun))))
		:initial))))


;;; Maybe-Let-Convert  --  Interface
;;;
;;;    This function is called when there is some reason to believe that
;;; the lambda Fun might be converted into a let.  This is done after local
;;; call analysis, and also when a reference is deleted.  We only convert to a
;;; let when the function is a normal local function, has no XEP, and is
;;; referenced in exactly one local call.  Conversion is also inhibited if the
;;; only reference is in a block about to be deleted.  We return true if we
;;; converted.
;;;
;;;    These rules may seem unnecessarily restrictive, since there are some
;;; cases where we could do the return with a jump that don't satisfy these
;;; requirements.  The reason for doing things this way is that it makes the
;;; concept of a let much more useful at the level of IR1 semantics.  The
;;; :ASSIGNMENT function kind provides another way to optimize calls to
;;; single-return/multiple call functions.
;;;
;;;    We don't attempt to convert calls to functions that have an XEP, since
;;; we might be embarrassed later when we want to convert a newly discovered
;;; local call.  Also, see OK-INITIAL-CONVERT-P.
;;;
(defun maybe-let-convert (fun)
  (declare (type clambda fun))
  (let ((refs (leaf-refs fun)))
    (when (and refs (null (rest refs))
	       (member (functional-kind fun) '(nil :assignment))
	       (not (functional-entry-function fun)))
      (let* ((ref-cont (node-cont (first refs)))
	     (dest (continuation-dest ref-cont)))
	(when (and dest
		   (basic-combination-p dest)
		   (eq (basic-combination-fun dest) ref-cont)
		   (eq (basic-combination-kind dest) :local)
		   (not (block-delete-p (node-block dest)))
		   (cond ((ok-initial-convert-p fun) t)
			 (t
			  (reoptimize-continuation ref-cont)
			  nil)))
	  (when (eq fun (node-home-lambda dest))
	    (delete-lambda fun)
	    (return-from maybe-let-convert nil))
	  (unless (eq (functional-kind fun) :assignment)
	    (let-convert fun dest))
	  (reoptimize-call dest)
	  (setf (functional-kind fun)
		(if (mv-combination-p dest) :mv-let :let))))
      t)))


;;;; Tail local calls and assignments:

;;; ONLY-HARMLESS-CLEANUPS  --  Internal
;;;
;;;    Return T if there are no cleanups between Block1 and Block2, or if they
;;; definitely won't generate any cleanup code.  Currently we recognize lexical
;;; entry points that are only used locally (if at all).
;;;
(defun only-harmless-cleanups (block1 block2)
  (declare (type cblock block1 block2))
  (or (eq block1 block2)
      (let ((cleanup2 (block-start-cleanup block2)))
	(do ((cleanup (block-end-cleanup block1)
		      (node-enclosing-cleanup (cleanup-mess-up cleanup))))
	    ((eq cleanup cleanup2) t)
	  (case (cleanup-kind cleanup)
	    ((:block :tagbody)
	     (unless (null (entry-exits (cleanup-mess-up cleanup)))
	       (return nil)))
	    (t (return nil)))))))


;;; MAYBE-CONVERT-TAIL-LOCAL-CALL  --  Interface
;;;
;;;    If a potentially TR local call really is TR, then convert it to jump
;;; directly to the called function.  We also call MAYBE-CONVERT-TO-ASSIGNMENT.
;;; The first value is true if we tail-convert.  The second is the value of
;;; M-C-T-A.  We can switch the succesor (potentially deleting the RETURN node)
;;; unless:
;;; -- The call has already been converted.
;;; -- The call isn't TR (random implicit MV PROG1.)
;;; -- The call is in an XEP (thus we might decide to make it non-tail so that
;;;    we can use known return inside the component.)
;;; -- There is a change in the cleanup between the call in the return, so we
;;;    might need to introduce cleanup code.
;;;
(defun maybe-convert-tail-local-call (call)
  (declare (type combination call))
  (let ((return (continuation-dest (node-cont call))))
    (assert (return-p return))
    (when (and (not (node-tail-p call))
	       (immediately-used-p (return-result return) call)
	       (not (eq (functional-kind (node-home-lambda call))
			:external))
	       (only-harmless-cleanups (node-block call)
				       (node-block return)))
      (node-ends-block call)
      (let ((block (node-block call))
	    (fun (combination-lambda call)))
	(setf (node-tail-p call) t)
	(unlink-blocks block (first (block-succ block)))
	(link-blocks block (node-block (lambda-bind fun)))
	(values t (maybe-convert-to-assignment fun))))))


;;; MAYBE-CONVERT-TO-ASSIGNMENT  --  Interface
;;;
;;;    Called when we believe it might make sense to convert Fun to an
;;; assignment.  All this function really does is determine when a function
;;; with more than one call can still be combined with the calling function's
;;; environment.  We can convert when:
;;; -- The function is a normal, non-entry function, and
;;; -- Except for one call, all calls must be tail recursive calls in the
;;;    called function (i.e. are self-recursive tail calls)
;;; -- OK-INITIAL-CONVERT-P is true.
;;;
;;;    There may be one outside call, and it need not be tail-recursive.  Since
;;; all tail local calls have already been converted to direct transfers, the
;;; only control semantics needed are to splice in the body at the non-tail
;;; call.  If there is no non-tail call, then we need only merge the
;;; environments.  Both cases are handled by LET-CONVERT.
;;;
;;; ### It would actually be possible to allow any number of outside calls as
;;; long as they all return to the same place (i.e. have the same conceptual
;;; continuation.)  A special case of this would be when all of the outside
;;; calls are tail recursive.
;;;
(defun maybe-convert-to-assignment (fun)
  (declare (type clambda fun))
  (when (and (not (functional-kind fun))
	     (not (functional-entry-function fun)))
    (let ((outside-non-tail-call nil)
	  (outside-call nil))
      (when (and (dolist (ref (leaf-refs fun) t)
		   (let ((dest (continuation-dest (node-cont ref))))
		     (when (or (not dest)
			       (block-delete-p (node-block dest)))
		       (return nil))
		     (let ((home (node-home-lambda ref)))
		       (unless (eq home fun)
			 (when outside-call
			   (return nil))
			 (setq outside-call dest))
		       (unless (node-tail-p dest)
			 (when (or outside-non-tail-call (eq home fun))
			   (return nil))
			 (setq outside-non-tail-call dest)))))
		 (ok-initial-convert-p fun))
	(setf (functional-kind fun) :assignment)
        (cond (outside-call
	       (setf (functional-kind fun) :assignment)
	       (let-convert fun outside-call)
	       (when outside-non-tail-call
		 (reoptimize-call outside-non-tail-call))
	       t)
              (t
	       (delete-lambda fun)
	       nil))))))
