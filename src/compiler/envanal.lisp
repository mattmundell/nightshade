;;; The environment analysis phase for the compiler.  This phase annotates
;;; IR1 with a hierarchy of environment structures, determining the
;;; environment that each Lambda allocates its variables and finding what
;;; values are closed over by each environment.

(in-package "C")

#[ Environment Analysis

    Determine which distinct environments need to be allocated, and what
    context needed to be closed over by each environment.  Detect non-local
    exits and set closure variables.  Also emit cleanup code as funny
    function calls.  This is the last pure ICR pass.

Phase position: 9/23 (front)

Presence: required

Files: envanal

Entry functions: `environment-analyze'

Call sequences:

    environment-analyze
      reinit-lambda-environment
        get-lambda-environment
      compute-closure
        note-unreferenced-vars
        close-over
      find-non-local-exits
        note-non-local-exit
      find-cleanup-points
        emit-cleanups
      tail-annotate


FIX where are environments introduced? partly in [Implicit Continuation Representation]

    A related change would be to annotate ICR with information about tail-recursion
    relations.  What we would do is add a slot to the node structure that points to
    the corresponding Tail-Info when a node is in a TR position.  This annotation
    would be made in a final ICR pass that runs after cleanup code is generated
    (part of environment analysis).  When true, the node is in a true TR position
    (modulo return-convention incompatibility).  When we determine return
    conventions, we null out the tail-p slots in XEP calls or known calls where we
    decided not to preserve tail-recursion.

    In this phase, we also check for changes in the dynamic binding environment
    that require cleanup code to be generated.  We just check for changes in the
    Continuation-Cleanup on local control transfers.  If it changes from
    an inner dynamic context to an outer one that is in the same environment, then
    we emit code to clean up the dynamic bindings between the old and new
    continuation.  We represent the result of cleanup detection to the back end by
    interposing a new block containing a call to a funny function.  Local exits
    from CATCH or UNWIND-PROTECT are detected in the same way.

The primary activity in environment analysis is the annotation of ICR with
environment structures describing where variables are allocated and what values
the environment closes over.

Each lambda points to the environment where its variables are allocated, and
the environments point back.  We always allocate the environment at the Bind
node for the sole non-let lambda in the environment, so there is a close
relationship between environments and functions.  Each "real function" (any
beside a `let') has a corresponding environment.

We attempt to share the same environment among as many lambdas as possible so
that unnecessary environment manipulation is not done.  During environment
analysis the only optimization of this sort is realizing that a Let (a lambda
with no Return node) cannot need its own environment, since there is no way
that it can return and discover that its old values have been clobbered.

When the function is called, values from other environments may need to be made
available in the function's environment.  These values are said to be "closed
over".

Even if a value is not referenced in a given environment, it may need to be
closed over in that environment so that it can be passed to a called function
that does reference the value.  When we discover that a value must be closed
over by a function, we must close over the value in all the environments where
that function is referenced.  This applies to all references, not just local
calls, since at other references we must have the values on hand so that we can
build a closure.  This propagation must be applied recursively, since the value
must also be available in *those* functions' callers.

If a closure reference is known to be "safe" (not an upward funarg), then the   ; FIX what's an upward funarg?
closure structure may be allocated on the stack.

Closure analysis deals only with closures over values, while Lisp requires
closures over variables.  The difference only becomes significant when
variables are set.  If a variable is only referenced, then we can freely
make copies of it without keeping track of where the copies are.  When a
variable is set, we must maintain a single value cell, or at least the
illusion thereof.  We achieve this by creating a heap-allocated "value
cell" structure for each set variable that is closed over.  The pointer to
this value cell is passed around as the "value" corresponding to that
variable.  References to the variable must explicitly indirect through the
value cell.

When we are scanning over the lambdas in the component, we also check for bound
but not referenced variables.

Environment analysis emits cleanup code for local exits and markers for
non-local exits.

A non-local exit is a control transfer from one environment to another.  In a
non-local exit, we must close over the continuation that we transfer to so that
the exiting function can find its way back.  We indicate the need to close a
continuation by placing the continuation structure in the closure and also
pushing it on a list in the environment structure for the target of the exit.
    XXX To be safe, we would treat the continuation as a set closure variable so
        that we could invalidate it when we leave the dynamic extent of the exit point.
        Transferring control to a meaningless stack pointer would be apt to cause
        horrible death.

Each local control transfer may require dynamic state such as special bindings
to be undone.  We represent cleanup actions by funny function calls in a new
block linked in as an implicit MV-PROG1.
]#

;;; Environment-Analyze  --  Interface
;;;
;;; Do environment analysis on the code in Component.  This involves various
;;; things:
;;;  1] Make an Environment structure for each non-let lambda, assigning the
;;;     lambda-environment for all lambdas.
;;;  2] Find all values that need to be closed over by each environment.
;;;  3] Scan the blocks in the component closing over non-local-exit
;;;     continuations.
;;;  4] Delete all non-top-level functions with no references.  This should
;;;     only get functions with non-NULL kinds, since normal functions are
;;;     deleted when their references go to zero.  If *byte-compiling*, then
;;;     don't delete optional entries with no references, since the byte
;;;     interpreter wants to call entries that the XEP doesn't.
;;;
(defun environment-analyze (component)
  (declare (type component component))
  (assert (every #'(lambda (x)
		     (eq (functional-kind x) :deleted))
		 (component-new-functions component)))
  (setf (component-new-functions component) ())
  (dolist (fun (component-lambdas component))
    (reinit-lambda-environment fun))
  (dolist (fun (component-lambdas component))
    (compute-closure fun)
    (dolist (let (lambda-lets fun))
      (compute-closure let)))

  (find-non-local-exits component)
  (find-cleanup-points component)
  (tail-annotate component)

  (dolist (fun (component-lambdas component))
    (when (null (leaf-refs fun))
      (let ((kind (functional-kind fun)))
	(unless (or (eq kind :top-level)
		    (and *byte-compiling* (eq kind :optional)))
	  (assert (member kind '(:optional :cleanup :escape)))
	  (setf (functional-kind fun) nil)
	  (delete-functional fun)))))

  (undefined-value))

;;; PRE-ENVIRONMENT-ANALYZE-TOP-LEVEL  --  Interface
;;;
;;; Called on component with top-level lambdas before the compilation of
;;; the associated non-top-level code to detect closed over top-level
;;; variables.  We just do COMPUTE-CLOSURE on all the lambdas.  This will
;;; pre-allocate environments for all the functions with closed-over
;;; top-level variables.  The post-pass will use the existing structure,
;;; rather than allocating a new one.  We return true if we discover any
;;; possible closure vars.
;;;
(defun pre-environment-analyze-top-level (component)
  (declare (type component component))
  (let ((found-it nil))
    (dolist (lambda (component-lambdas component))
      (when (compute-closure lambda)
	(setq found-it t))
      (dolist (let (lambda-lets lambda))
	(when (compute-closure let)
	  (setq found-it t))))
    found-it))

;;; GET-LAMBDA-ENVIRONMENT  --  Internal
;;;
;;; If Fun has an environment, return it, otherwise assign one.
;;;
(defun get-lambda-environment (fun)
  (declare (type clambda fun))
  (let* ((fun (lambda-home fun))
	 (env (lambda-environment fun)))
    (or env
	(let ((res (make-environment :function fun)))
	  (setf (lambda-environment fun) res)
	  (dolist (lambda (lambda-lets fun))
	    (setf (lambda-environment lambda) res))
	  res))))

;;; REINIT-LAMBDA-ENVIRONMENT  --  Internal
;;;
;;; If Fun has no environment, assign one, otherwise clean up variables
;;; that have no sets or refs.  If a var has no references, we remove it
;;; from the closure.  If it has no sets, we clear the INDIRECT flag.  This
;;; is necessary because pre-analysis is done before optimization.
;;;
(defun reinit-lambda-environment (fun)
  (let ((old (lambda-environment (lambda-home fun))))
    (cond (old
	   (setf (environment-closure old)
		 (delete-if #'(lambda (x)
				(and (lambda-var-p x)
				     (null (leaf-refs x))))
			    (environment-closure old)))
	   (flet ((clear (fun)
		    (dolist (var (lambda-vars fun))
		      (unless (lambda-var-sets var)
			(setf (lambda-var-indirect var) nil)))))
	     (clear fun)
	     (dolist (let (lambda-lets fun))
	       (clear let))))
	  (t
	   (get-lambda-environment fun))))
  (undefined-value))

;;; GET-NODE-ENVIRONMENT  --  Internal
;;;
;;; Get node's environment, assigning one if necessary.
;;;
(defun get-node-environment (node)
  (declare (type node node))
  (get-lambda-environment (node-home-lambda node)))

;;; Compute-Closure  --  Internal
;;;
;;; Find any variables in Fun with references outside of the home
;;; environment and close over them.  If a closed over variable is set,
;;; then we set the Indirect flag so that we will know the closed over
;;; value is really a pointer to the value cell.  We also warn about
;;; unreferenced variables here, just because it's a convenient place to do
;;; it.  We return true if we close over anything.
;;;
(defun compute-closure (fun)
  (declare (type clambda fun))
  (let ((env (get-lambda-environment fun))
	(did-something nil))
    (note-unreferenced-vars fun)
    (dolist (var (lambda-vars fun))
      (dolist (ref (leaf-refs var))
	(let ((ref-env (get-node-environment ref)))
	  (unless (eq ref-env env)
	    (when (lambda-var-sets var)
	      (setf (lambda-var-indirect var) t))
	    (setq did-something t)
	    (close-over var ref-env env))))
      (dolist (set (basic-var-sets var))
	(let ((set-env (get-node-environment set)))
	  (unless (eq set-env env)
	    (setq did-something t)
	    (setf (lambda-var-indirect var) t)
	    (close-over var set-env env)))))
    did-something))

;;; Close-Over  --  Internal
;;;
;;; Make sure that Thing is closed over in Ref-Env and in all environments
;;; for the functions that reference Ref-Env's function (not just calls.)
;;; Home-Env is Thing's home environment.  When we reach the home
;;; environment, we stop propagating the closure.
;;;
(defun close-over (thing ref-env home-env)
  (declare (type environment ref-env home-env))
  (cond ((eq ref-env home-env))
	((member thing (environment-closure ref-env)))
	(t
	 (push thing (environment-closure ref-env))
	 (dolist (call (leaf-refs (environment-function ref-env)))
	   (close-over thing (get-node-environment call) home-env))))
  (undefined-value))


;;;; Non-local exit.

;;; Insert-NLX-Entry-Stub  --  Internal
;;;
;;; Insert the entry stub before the original exit target, and add a new
;;; entry to the Environment-Nlx-Info.  The %NLX-Entry call in the stub is
;;; passed the NLX-Info as an argument so that the back end knows what
;;; entry is being done.
;;;
;;; The link from the Exit block to the entry stub is changed to be a link to
;;; the component head.  Similarly, the Exit block is linked to the component
;;; tail.  This leaves the entry stub reachable, but makes the flow graph less
;;; confusing to flow analysis.
;;;
;;; If a catch or an unwind-protect, then we set the Lexenv for the last node
;;; in the cleanup code to be the enclosing environment, to represent the fact
;;; that the binding was undone as a side-effect of the exit.  This will cause
;;; a lexical exit to be broken up if we are actually exiting the scope (i.e.
;;; a BLOCK), and will also do any other cleanups that may have to be done on
;;; the way.
;;;
(defun insert-nlx-entry-stub (exit env)
  (declare (type environment env) (type exit exit))
  (let* ((exit-block (node-block exit))
	 (next-block (first (block-succ exit-block)))
	 (cleanup (entry-cleanup (exit-entry exit)))
	 (info (make-nlx-info :cleanup cleanup
			      :continuation (node-cont exit)))
	 (entry (exit-entry exit))
	 (new-block (insert-cleanup-code exit-block next-block
					 entry
					 `(%nlx-entry ',info)
					 (entry-cleanup entry)))
	 (component (block-component new-block)))
    (unlink-blocks exit-block new-block)
    (link-blocks exit-block (component-tail component))
    (link-blocks (component-head component) new-block)

    (setf (nlx-info-target info) new-block)
    (push info (environment-nlx-info env))
    (push info (cleanup-nlx-info cleanup))
    (when (member (cleanup-kind cleanup) '(:catch :unwind-protect))
      (setf (node-lexenv (block-last new-block))
	    (node-lexenv entry))))

  (undefined-value))

;;; Note-Non-Local-Exit  --  Internal
;;;
;;; Do stuff necessary to represent a non-local exit from the node Exit
;;; into Env.  This is called for each non-local exit node, of which there
;;; may be several per exit continuation.  This is what we do:
;;; -- If there isn't any NLX-Info entry in the environment, make an entry
;;;    stub, otherwise just move the exit block link to the component tail.
;;; -- Close over the NLX-Info in the exit environment.
;;; -- If the exit is from an :Escape function, then substitute a constant
;;;    reference to NLX-Info structure for the escape function reference.  This
;;;    will cause the escape function to be deleted (although not removed from
;;;    the DFO.)  The escape function is no longer needed, and we don't want to
;;;    emit code for it.  We then also change the %NLX-ENTRY call to use
;;;    the NLX continuation so that there will be a use to represent the NLX
;;;    use.
;;;
(defun note-non-local-exit (env exit)
  (declare (type environment env) (type exit exit))
  (let ((entry (exit-entry exit))
	(cont (node-cont exit))
	(exit-fun (node-home-lambda exit)))

    (if (find-nlx-info entry cont)
	(let ((block (node-block exit)))
	  (assert (= (length (block-succ block)) 1))
	  (unlink-blocks block (first (block-succ block)))
	  (link-blocks block (component-tail (block-component block))))
	(insert-nlx-entry-stub exit env))

    (let ((info (find-nlx-info entry cont)))
      (assert info)
      (close-over info (node-environment exit) env)
      (when (eq (functional-kind exit-fun) :escape)
	(mapc #'(lambda (x)
		  (setf (node-derived-type x) *wild-type*))
	      (leaf-refs exit-fun))
	(substitute-leaf (find-constant info) exit-fun)
	(let ((node (block-last (nlx-info-target info))))
	  (delete-continuation-use node)
	  (add-continuation-use node (nlx-info-continuation info))))))

  (undefined-value))

;;; Find-Non-Local-Exits  --  Internal
;;;
;;; Iterate over the Exits in Component, calling Note-Non-Local-Exit when
;;; we find a block that ends in a non-local Exit node.  We also ensure
;;; that all Exit nodes are either non-local or degenerate by calling
;;; IR1-Optimize-Exit on local exits.  This makes life simpler for later
;;; phases.
;;;
(defun find-non-local-exits (component)
  (declare (type component component))
  (dolist (lambda (component-lambdas component))
    (dolist (entry (lambda-entries lambda))
      (dolist (exit (entry-exits entry))
	(let ((target-env (node-environment entry)))
	  (if (eq (node-environment exit) target-env)
	      (unless *converting-for-interpreter*
		(maybe-delete-exit exit))
	      (note-non-local-exit target-env exit))))))

  (undefined-value))


;;;; Cleanup emission.

;;; Emit-Cleanups  --  Internal
;;;
;;; Zoom up the cleanup nesting until we hit Cleanup1, accumulating cleanup
;;; code as we go.  When we are done, convert the cleanup code in an
;;; implicit MV-Prog1.  We have to force local call analysis of new
;;; references to Unwind-Protect cleanup functions.  If we don't actually
;;; have to do anything, then we don't insert any cleanup code.
;;;
;;; If we do insert cleanup code, we check that Block1 doesn't end in a "tail"
;;; local call.
;;;
;;; We don't need to adjust the ending cleanup of the cleanup block, since
;;; the cleanup blocks are inserted at the start of the DFO, and are thus
;;; never scanned.
;;;
(defun emit-cleanups (block1 block2)
  (declare (type cblock block1 block2))
  (collect ((code)
	    (reanalyze-funs))
    (let ((cleanup2 (block-start-cleanup block2)))
      (do ((cleanup (block-end-cleanup block1)
		    (node-enclosing-cleanup (cleanup-mess-up cleanup))))
	  ((eq cleanup cleanup2))
	(let* ((node (cleanup-mess-up cleanup))
	       (args (when (basic-combination-p node)
		       (basic-combination-args node))))
	  (ecase (cleanup-kind cleanup)
	    (:special-bind
	     (code `(%special-unbind ',(continuation-value (first args)))))
	    (:catch
	     (code `(%catch-breakup)))
	    (:unwind-protect
	     (code `(%unwind-protect-breakup))
	     (let ((fun (ref-leaf (continuation-use (second args)))))
	       (reanalyze-funs fun)
	       (code `(%funcall ,fun))))
	    ((:block :tagbody)
	     (cond (*converting-for-interpreter*
		    ;; Avoid duplicates, as the interpreter expects just one
		    ;; %lexical-exit-breakup for each entry.
		    (let ((nlx (first (cleanup-nlx-info cleanup))))
		      (when nlx
			(code `(%lexical-exit-breakup ',nlx)))))
		   (t
		    (dolist (nlx (cleanup-nlx-info cleanup))
		      (code `(%lexical-exit-breakup ',nlx)))))))))

      (when (code)
	(assert (not (node-tail-p (block-last block1))))
	(insert-cleanup-code block1 block2
			     (block-last block1)
			     `(progn ,@(code)))
	(dolist (fun (reanalyze-funs))
	  (local-call-analyze-1 fun)))))

  (undefined-value))

;;; Find-Cleanup-Points  --  Internal
;;;
;;; Loop over the blocks in component, calling Emit-Cleanups when we see a
;;; successor in the same environment with a different cleanup.  We ignore
;;; the cleanup transition if it is to a cleanup enclosed by the current
;;; cleanup, since in that case we are just messing up the environment,
;;; hence this is not the place to clean it.
;;;
(defun find-cleanup-points (component)
  (declare (type component component))
  (do-blocks (block1 component)
    (let ((env1 (block-environment block1))
	  (cleanup1 (block-end-cleanup block1)))
      (dolist (block2 (block-succ block1))
	(when (block-start block2)
	  (let ((env2 (block-environment block2))
		(cleanup2 (block-start-cleanup block2)))
	    (unless (or (not (eq env2 env1))
			(eq cleanup1 cleanup2)
			(and cleanup2
			     (eq (node-enclosing-cleanup
				  (cleanup-mess-up cleanup2))
				 cleanup1)))
	      (emit-cleanups block1 block2)))))))
  (undefined-value))

;;; Tail-Annotate  --  Internal
;;;
;;; Mark all tail-recursive uses of function result continuations with the
;;; corresponding tail-set.  Nodes whose type is NIL (i.e. don't return)
;;; such as calls to ERROR are never annotated as tail in order to preserve
;;; debugging information.
;;;
(defun tail-annotate (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (let ((ret (lambda-return fun)))
      (when ret
	(let ((result (return-result ret)))
	  (do-uses (use result)
	    (when (and (immediately-used-p result use)
		     (or (not (eq (node-derived-type use) *empty-type*))
			 (not (basic-combination-p use))
			 (eq (basic-combination-kind use) :local)))
		(setf (node-tail-p use) t)))))))
  (undefined-value))
