;;; The IR1 optimization phase of the compiler.  IR1 optimization is a
;;; grab-bag of optimizations that don't make major changes to the
;;; block-level control flow and don't use flow analysis.  These
;;; optimizations can mostly be classified as "meta-evaluation", but there
;;; is a sizable top-down component as well.

(in-package :c)

#[ ICR Optimize

   FIX where are function attributes and known funs intro'd?

    A grab-bag of all the non-flow ICR optimizations.  Fold constant
    functions, propagate types and eliminate code that computes unused
    values.  Special-case calls to some known global functions by replacing
    them with a computed function.  Merge blocks and eliminate `if'-`if's.
    Substitute `let' variables.

Phase position: 4/23 (front)

Presence: optional

Files: ir1opt, ir1tran, typetran, seqtran, vm/vm-tran
    FIX maybe split into them (? this node?)

Entry functions: `ir1-optimize'

Call sequences:

    ir1-optimize
      join-successor-if-possible
	join-blocks
      ir1-optimize-block
	ir1-optimize-combination
	  constant-fold-call
	ir1-optimize-if
	  convert-if-if
	ir1-optimize-return
	ir1-optimize-mv-combination
	ir1-optimize-set


XXX Somewhere describe basic ICR utilities: continuation-type,
    constant-continuation-p, etc.  Perhaps group by type in ICR description?

We are conservative about doing variable-for-variable substitution in ICR
optimization, since if we substitute a variable with a less restrictive type,
then we may prevent use of a "good" representation within the scope of the
inner binding.

Note that variable-variable substitutions aren't really crucial in ICR, since
they don't create opportunities for new optimizations (unlike substitution of
constants and functions).  A spurious variable-variable binding will show up as
a Move operation in VMR.  This can be optimized away by reaching-definitions
and also by targeting.
    XXX But actually, some optimizers do see if operands are the same variable.

    The IF-IF optimization can be modeled as a value driven optimization, since
    adding a use definitely is cause for marking the continuation for
    reoptimization.  [When do we add uses?  Let conversion is the only obvious
    time.]  I guess IF-IF conversion could also be triggered by a non-immediate use
    of the test continuation becoming immediate, but to allow this to happen would
    require Delete-Block (or somebody) to mark block-starts as needing to be
    reoptimized when a predecessor changes.  It's not clear how important it is
    that IF-IF conversion happen under all possible circumstances, as long as it
    happens to the obvious cases.

	XXX It isn't totally true that code flushing never enables other worthwhile
	optimizations.  Deleting a functional reference can cause a function to cease
	being an XEP, or even trigger let conversion.  It seems we still want to flush
	code during ICR optimize, but maybe we want to interleave it more intimately
	with the optimization pass. (FIX maybe this continued below)

    Ref-flushing works just as well forward as backward, so it could be done in the
    forward pass.  Call flushing doesn't work so well, but we could scan the block
    backward looking for any new flushable stuff if we flushed a call on the
    forward pass.

    When we delete a variable due to lack of references, we leave the variable
    in the lambda-list so that positional references still work.  The initial value
    continuation is flushed, though (replaced with NIL) allowing the initial value
    for to be deleted (modulo side-effects.)

    Note that we can delete vars with no refs even when they have sets.  I guess
    when there are no refs, we should also flush all sets, allowing the value
    expressions to be flushed as well.

    Squeeze out single-reference unset let variables by changing the dest of the
    initial value continuation to be the node that receives the ref.  This can be
    done regardless of what the initial value form is, since we aren't actually
    moving the evaluation.  Instead, we are in effect using the continuation's
    locations in place of the temporary variable.

    Doing this is of course, a wild violation of stack discipline, since the ref
    might be inside a loop, etc.  But with the VMR back-end, we only need to
    preserve stack discipline for unknown-value continuations; this ICR
    transformation must be already be inhibited when the DEST of the REF is a
    multiple-values receiver (EXIT, RETURN or MV-COMBINATION), since we must
    preserve the single-value semantics of the let-binding in this case.

    The REF and variable must be deleted as part of this operation, since the ICR
    would otherwise be left in an inconsistent state; we can't wait for the REF to
    be deleted due to bing unused, since we have grabbed the arg continuation and
    substituted it into the old DEST.

    The big reason for doing this transformation is that in macros such as INCF and
    PSETQ, temporaries are squeezed out, and the new value expression is evaluated
    directly to the setter, allowing any result type assertion to be applied to the
    expression evaluation.  Unlike in the case of substitution, there is no point
    in inhibiting this transformation when the initial value type is weaker than
    the variable type.  Instead, we intersect the asserted type for the old REF's
    CONT with the type assertion on the initial value continuation.  Note that the
    variable's type has already been asserted on the initial-value continuation.

    Of course, this transformation also simplifies the ICR even when it doesn't
    discover interesting type assertions, so it makes sense to do it whenever
    possible.  This reduces the demands placed on register allocation, etc.

There are three dead-code flushing rules:
 # Refs with no DEST may be flushed.
 # Known calls with no dest that are flushable may be flushed.  We null the
   DEST in all the args.
 # If a lambda-var has no refs, then it may be deleted.  The flushed argument
   continuations have their DEST nulled.

These optimizations all enable one another.  We scan blocks backward, looking
for nodes whose CONT has no DEST, then type-dispatching off of the node.  If we
delete a ref, then we check to see if it is a lambda-var with no refs.  When we
flush an argument, we mark the blocks for all uses of the CONT as needing to be
reoptimized.


== Goals for ICR optimizations ==

    When an optimization is disabled, code should still be correct and not
    ridiculously inefficient.  Phases shouldn't be made mandatory when they have
    lots of non-required stuff jammed into them.

This pass is optional, but is desirable if anything is more important than
compilation speed.

This phase is a grab-bag of optimizations that concern themselves with the
flow of values through the code representation.  The main things done are
type inference, constant folding and dead expression elimination.  Type
inference is bottom-up inference of node types from context.  This phase
can be understood as a walk of the expression tree that propagates
assertions down the tree and propagates derived information up the tree.
The main complication is that there isn't any expression tree, since ICR is
flow-graph based.

We repeat this pass until we don't discover anything new.  This is a bit of
a feat, since we dispatch to arbitrary functions which may do arbitrary
things, making it hard to tell if anything really happened.  Even if we
solve this problem by requiring people to flag when they changed or by
checking to see if they changed something, there are serious efficiency
problems due to massive redundant computation, since in many cases the only
way to tell if anything changed is to recompute the value and see if it is
different from the old one.

We solve this problem by requiring that optimizations for a node only
depend on the properties of the CONT and the continuations that have the
node as their DEST.  If the continuations haven't changed since the last
pass, then we don't attempt to re-optimize the node, since we know nothing
interesting will happen.

We keep track of which continuations have changed by a REOPTIMIZE flag that
is set whenever something about the continuation's value changes.

When doing the bottom up pass, we dispatch to type specific code that knows how
to tell when a node needs to be reoptimized and does the optimization.  These
node types are special-cased: COMBINATION, IF, RETURN, EXIT, SET.

The REOPTIMIZE flag in the COMBINATION-FUN is used to detect when the
function information might have changed, so that we know when there are new
assertions that could be propagated from the function type to the
arguments.

When we discover something about a leaf, or substitute for leaf, we
reoptimize the CONT for all the REF and SET nodes.

We have flags in each block that indicate when any nodes or continuations
in the block need to be re-optimized, so we don't have to scan blocks where
there is no chance of anything happening.

It is important for efficiency purposes that optimizers never say that they
did something when they didn't, but this by itself doesn't guarantee timely
termination.  I believe that with the type system implemented, type
inference will converge in finite time, but as a practical matter, it can
take far too long to discover not much.  For this reason, ICR optimization
is terminated after three consecutive passes that don't add or delete code.
This premature termination only happens 2% of the time.


== Flow graph simplification ==

Things done:
    Delete blocks with no predecessors.
    Merge blocks that can be merged.
    Convert local calls to Let calls.
    Eliminate degenerate IFs.

We take care not to merge blocks that are in different functions or have
different cleanups.  This guarantees that non-local exits are always at block
ends and that cleanup code never needs to be inserted within a block.

We eliminate IFs with identical consequent and alternative.  This would most
likely happen if both the consequent and alternative were optimized away.

 XXX Could also be done if the consequent and alternative were different blocks,
     but computed the same value.  This could be done by a sort of
     cross-jumping optimization that looked at the predecessors for a block
     and merged code shared between predecessors.  IFs with identical
     branches would eventually be left with nothing in their branches.

We eliminate IF-IF constructs:
    (IF (IF A B C) D E) =>
    (IF A (IF B D E) (IF C D E))

In reality, what we do is replicate blocks containing only an IF node where the
predicate continuation is the block start.  We make one copy of the IF node for
each use, leaving the consequent and alternative the same.  If you look at the
flow graph representation, you will see that this is really the same thing as
the above source to source transformation.


== Forward ICR optimizations ==

In the forward pass, we scan the code in forward depth-first order.  We
examine each call to a known function, and:

  * Eliminate any bindings for unused variables.

  * Do top-down type assertion propagation.  In local calls, we propagate
    asserted and derived types between the call and the called lambda.

  * Replace calls of foldable functions with constant arguments with the
    result.  We don't have to actually delete the call node, since Top-Down
    optimize will delete it now that its value is unused.

  * Run any Optimizer for the current function.  The optimizer does arbitrary
    transformations by hacking directly on the IR.  This is useful primarily
    for arithmetic simplification and similar things that may need to examine
    and modify calls other than the current call.  The optimizer is responsible
    for recording any changes that it makes.  An optimizer can inhibit further
    optimization of the node during the current pass by returning true.  This
    is useful when deleting the node.

  * Do ICR transformations, replacing a global function call with equivalent
    inline lisp code.

  * Do bottom-up type propagation/inferencing.  For some functions such as
    Coerce we will dispatch to a function to find the result type.  The
    Derive-Type function just returns a type structure, and we check if it is
    different from the old type in order to see if there was a change.

  * Eliminate IFs with predicates known to be true or false.

  * Substitute the value for unset let variables that are bound to constants,
    unset lambda variables or functionals.  FIX first mention of functional?

  * Propagate types from local call args to var refs.

We use type info from the function continuation to find result types for
functions that don't have a derive-type method.

=== ICR transformation ===

ICR transformation does "source to source" transformations on known global
functions, taking advantage of semantic information such as argument types and
constant arguments.  Transformation is optional, but should be done if speed or
space is more important than compilation speed.  Transformations which increase
space should pass when space is more important than speed.

A transform is actually an inline function call where the function is computed
at compile time.  The transform gets to peek at the continuations for the
arguments, and computes a function using the information gained.  Transforms
should be cautious about directly using the values of constant continuations,
since the compiler must preserve eqlness of named constants, and it will have a
hard time if transforms go around randomly copying constants.

The lambda that the transform computes replaces the original function variable
reference as the function for the call.  This lets the compiler worry about
evaluating each argument once in the right order.  We want to be careful to
preserve type information when we do a transform, since it may be less than
obvious what the transformed code does.

There can be any number of transforms for a function.  Each transform is
associated with a function type that the call must be compatible with.  A
transform is only invoked if the call has the right type.  This provides a way
to deal with the common case of a transform that only applies when the
arguments are of certain types and some arguments are not specified.  We always
use the derived type when determining whether a transform is applicable.  Type
check is responsible for setting the derived type to the intersection of the
asserted and derived types.

If the code in the expansion has insufficient explicit or implicit argument
type checking, then it should cause checks to be generated by making
declarations.

A transformation may decide to pass if it doesn't like what it sees when it
looks at the args.  The Give-Up function unwinds out of the transform and deals
with complaining about inefficiency if speed is more important than brevity.
The format args for the message are arguments to Give-Up.  If a transform can't
be done, we just record the message where ICR finalize can find it.  note.  We   ; FIX?
can't complain immediately, since it might get transformed later on.


== Backward ICR optimizations ==

In the backward pass, we scan each block in reverse order, and
eliminate any effectless nodes with unused values.  In ICR this is the
only way that code is deleted other than the elimination of unreachable blocks.
]#


;;;; Interface for obtaining results of constant folding.

;;; Constant-Continuation-P  --  Interface
;;;
;;; Return true if the sole use of Cont is a reference to a constant leaf.
;;;
(defun constant-continuation-p (cont)
  (declare (type continuation cont) (values boolean))
  (let ((use (continuation-use cont)))
    (and (ref-p use)
	 (constant-p (ref-leaf use)))))

;;; Continuation-Value  --  Interface
;;;
;;; Return the constant value for a continuation whose only use is a
;;; constant node.
;;;
(defun continuation-value (cont)
  (declare (type continuation cont))
  (assert (constant-continuation-p cont))
  (constant-value (ref-leaf (continuation-use cont))))


;;;; Interface for obtaining results of type inference.

;;; CONTINUATION-PROVEN-TYPE  --  Interface
;;;
;;; Return a (possibly values) type that describes what we have proven
;;; about the type of Cont without taking any type assertions into
;;; consideration.  This is just the union of the NODE-DERIVED-TYPE of all
;;; the uses.  Most often people use CONTINUATION-DERIVED-TYPE or
;;; CONTINUATION-TYPE instead of using this function directly.
;;;
(defun continuation-proven-type (cont)
  (declare (type continuation cont))
  (ecase (continuation-kind cont)
    ((:block-start :deleted-block-start)
     (let ((uses (block-start-uses (continuation-block cont))))
       (if uses
	   ; FIX while
	   (do ((res (node-derived-type (first uses))
		     (values-type-union (node-derived-type (first current))
					res))
		(current (rest uses) (rest current)))
	       ((null current) res))
	   *empty-type*)))
    (:inside-block
     (node-derived-type (continuation-use cont)))))


;;; Continuation-Derived-Type  --  Interface
;;;
;;; Our best guess for the type of this continuation's value.  Note that
;;; this may be (FIX a?) Values or Function type, which cannot be passed as an
;;; argument to the normal type operations.  See Continuation-Type.  This
;;; may be called on deleted continuations, always returning *.
;;;
;;; What we do is call CONTINUATION-PROVEN-TYPE and check whether the
;;; result is a subtype of the assertion (FIX what assertn?).  If so, return the proven type
;;; and set TYPE-CHECK to nil.  Otherwise, return the intersection of the
;;; asserted and proven types, and set TYPE-CHECK T.  If TYPE-CHECK already
;;; has a non-null value, then preserve it.  Only in the somewhat unusual
;;; circumstance of a newly discovered assertion will we change TYPE-CHECK
;;; from NIL to T.
;;;
;;; The result value is cached in the Continuation-%Derived-Type.  If the
;;; slot is true, just return that value, otherwise recompute and stash the
;;; value there.
;;;
(proclaim '(inline continuation-derived-type))
(defun continuation-derived-type (cont)
  (declare (type continuation cont))
  (or (continuation-%derived-type cont)
      (%continuation-derived-type cont)))
;;;
(defun %continuation-derived-type (cont)
  (declare (type continuation cont))
  (let ((proven (continuation-proven-type cont))
	(asserted (continuation-asserted-type cont)))
    (cond ((values-subtypep proven asserted)
	   (setf (continuation-%type-check cont) nil)
	   (setf (continuation-%derived-type cont) proven))
	  (t
	   (unless (or (continuation-%type-check cont)
		       (not (continuation-dest cont))
		       (eq asserted *universal-type*))
	     (setf (continuation-%type-check cont) t))

	   (setf (continuation-%derived-type cont)
		 (values-type-intersection asserted proven))))))

;;; CONTINUATION-TYPE-CHECK  --  Interface
;;;
;;; Call CONTINUATION-DERIVED-TYPE to make sure the slot is up to date,
;;; then return it.
;;;
(proclaim '(inline continuation-type-check))
(defun continuation-type-check (cont)
  (declare (type continuation cont))
  (continuation-derived-type cont)
  (continuation-%type-check cont))

;;; Continuation-Type  --  Interface
;;;
;;; Return the derived type for Cont's first value.  This is guaranteed not
;;; to be a Values or Function type.
;;;
(defun continuation-type (cont)
  (declare (type continuation cont) (values ctype))
  (single-value-type (continuation-derived-type cont)))


;;;; Interface routines used by optimizers.

;;; Reoptimize-Continuation  --  Interface
;;;
;;; This function is called by optimizers to indicate that something
;;; interesting has happened to the value of Cont.  Optimizers must make
;;; sure that they don't call for reoptimization when nothing has happened,
;;; since optimization will fail to terminate.
;;;
;;; We clear any cached type for the continuation and set the reoptimize
;;; flags on everything in sight, unless the continuation is deleted (in
;;; which case we do nothing.)
;;;
;;; Since this can get called curing IR1 conversion, we have to be careful
;;; not to fly into space when the Dest's Prev is missing.
;;;
(defun reoptimize-continuation (cont)
  (declare (type continuation cont))
  (unless (member (continuation-kind cont) '(:deleted :unused))
    (setf (continuation-%derived-type cont) nil)
    (let ((dest (continuation-dest cont)))
      (when dest
	(setf (continuation-reoptimize cont) t)
	(setf (node-reoptimize dest) t)
	(let ((prev (node-prev dest)))
	  (when prev
	    (let* ((block (continuation-block prev))
		   (component (block-component block)))
	      (when (typep dest 'cif)
		(setf (block-test-modified block) t))
	      (setf (block-reoptimize block) t)
	      (setf (component-reoptimize component) t))))))
    (do-uses (node cont)
      (setf (block-type-check (node-block node)) t)))
  (undefined-value))

;;; Derive-Node-Type  --  Interface
;;;
;;; Annotate Node to indicate that its result has been proven to be typep
;;; to RType.  After IR1 conversion has happened, this is the only correct
;;; way to supply information discovered about a node's type.  If FIX you fuck
;;; with the Node-Derived-Type directly, then information may be lost and
;;; reoptimization may not happen.
;;;
;;; What we do is intersect Rtype with Node's Derived-Type.  If the
;;; intersection is different from the old type, then we do a
;;; Reoptimize-Continuation on the Node-Cont.
;;;
(defun derive-node-type (node rtype)
  (declare (type node node) (type ctype rtype))
  (let ((node-type (node-derived-type node)))
    (unless (eq node-type rtype)
      (let ((int (values-type-intersection node-type rtype)))
	(when (type/= node-type int)
	  (when (and *check-consistency*
		     (eq int *empty-type*)
		     (not (eq rtype *empty-type*)))
	    (let ((*compiler-error-context* node))
	      (compiler-warning
	       "New inferred type ~S conflicts with old type:~
		~%  ~S~%*** Bug?"
	       (type-specifier rtype) (type-specifier node-type))))
	  (setf (node-derived-type node) int)
	  (reoptimize-continuation (node-cont node))))))
  (undefined-value))

(declaim (start-block assert-continuation-type
		      assert-continuation-optional-type assert-call-type))

;;; Assert-Continuation-Type  --  Interface
;;;
;;; Similar to Derive-Node-Type, but asserts that it is an error for Cont's
;;; value not to be typep to Type.  If we improve the assertion, we set
;;; TYPE-CHECK and TYPE-ASSERTED to guarantee that the new assertion will
;;; be checked.
;;;
(defun assert-continuation-type (cont type)
  (declare (type continuation cont) (type ctype type))
  (let ((cont-type (continuation-asserted-type cont)))
    (unless (eq cont-type type)
      (let ((int (values-type-intersection cont-type type)))
	(when (type/= cont-type int)
	  (setf (continuation-asserted-type cont) int)
	  (do-uses (node cont)
	    (setf (block-attributep (block-flags (node-block node))
				    type-check type-asserted)
		  t))
	  (reoptimize-continuation cont)))))
  (undefined-value))

;;; Assert-continuation-optional-type  --  Interface
;;;
;;; Similar to Assert-Continuation-Type, but asserts that the type is for
;;; an optional argument and that other arguments may be received.
;;;
(defun assert-continuation-optional-type (cont type)
  (declare (type continuation cont) (type ctype type))
  (let ((opt-type (make-values-type :optional (list type)
				    :rest *universal-type*)))
    (assert-continuation-type cont opt-type)))


;;; Assert-Call-Type  --  Interface
;;;
;;; Assert that Call is to a function of the specified Type.  It is assumed
;;; that the call is legal and has only constants in the keyword positions.
;;;
(defun assert-call-type (call type)
  (declare (type combination call) (type function-type type))
  (derive-node-type call (function-type-returns type))
  (let ((args (combination-args call)))
    (dolist (req (function-type-required type))
      (when (null args) (return-from assert-call-type))
      (let ((arg (pop args)))
	(assert-continuation-optional-type arg req)))
    (dolist (opt (function-type-optional type))
      (when (null args) (return-from assert-call-type))
      (let ((arg (pop args)))
	(assert-continuation-optional-type arg opt)))

    (let ((rest (function-type-rest type)))
      (when rest
	(dolist (arg args)
	  (assert-continuation-optional-type arg rest))))

    (dolist (key (function-type-keywords type))
      (let ((name (key-info-name key)))
	(do ((arg args (cddr arg)))
	    ((null arg))
	  (when (eq (continuation-value (first arg)) name)
	    (assert-continuation-optional-type
	     (second arg) (key-info-type key)))))))
  (undefined-value))


;;;; IR1-OPTIMIZE.

(declaim (start-block ir1-optimize))

;;; IR1-Optimize  --  Interface
;;;
;;; Do one forward pass over Component, deleting unreachable blocks and
;;; doing IR1 optimizations.  We can ignore all blocks that don't have the
;;; Reoptimize flag set.  If Component-Reoptimize is true when we are done,
;;; then another iteration would be beneficial.
;;;
;;; We delete blocks when there is either no predecessor or the block is in
;;; a lambda that has been deleted.  These blocks would eventually be
;;; deleted by DFO recomputation, but doing it here immediately makes the
;;; effect available to IR1 optimization.
;;;
(defun ir1-optimize (component)
  (declare (type component component))
  (setf (component-reoptimize component) nil)
  (do-blocks (block component)
    (cond
     ((or (block-delete-p block)
	  (null (block-pred block))
	  (eq (functional-kind (block-home-lambda block)) :deleted))
      (delete-block block))
     (t
      (loop
	(let ((succ (block-succ block)))
	  (unless (and succ (null (rest succ)))
	    (return)))

	(let ((last (block-last block)))
	  (typecase last
	    (cif
	     (flush-dest (if-test last))
	     (when (unlink-node last) (return)))
	    (exit
	     (when (maybe-delete-exit last) (return)))))

	(unless (join-successor-if-possible block)
	  (return)))

      (when (and (block-reoptimize block) (block-component block))
	(assert (not (block-delete-p block)))
	(ir1-optimize-block block))

      (when (and (block-flush-p block) (block-component block))
	(assert (not (block-delete-p block)))
	(flush-dead-code block)))))

  (undefined-value))

;;; Ir1-Optimize-Block  --  Internal
;;;
;;; Loop over the nodes in Block, looking for stuff that needs to be
;;; optimized.  We dispatch off of the type of each node with its reoptimize
;;; flag set:
;;; -- With a combination, we call Propagate-Function-Change whenever the
;;;    function changes, and call IR1-Optimize-Combination if any argument
;;;    changes.
;;; -- With an Exit, we derive the node's type from the Value's type.  We don't
;;;    propagate Cont's assertion to the Value, since if we did, this would
;;;    move the checking of Cont's assertion to the exit.  This wouldn't work
;;;    with Catch and UWP, where the Exit node is just a placeholder for the
;;;    actual unknown exit.
;;;
;;; Note that we clear the node & block reoptimize flags *before* doing the
;;; optimization.  This ensures that the node or block will be reoptimized if
;;; necessary.  We leave the NODE-OPTIMIZE flag set going into
;;; IR1-OPTIMIZE-RETURN, since it wants to clear the flag itself.
;;;
(defun ir1-optimize-block (block)
  (declare (type cblock block))
  (setf (block-reoptimize block) nil)
  (do-nodes (node cont block :restart-p t)
    (when (node-reoptimize node)
      (setf (node-reoptimize node) nil)
      (typecase node
	(ref)
	(combination
	 (ir1-optimize-combination node))
	(cif
	 (ir1-optimize-if node))
	(creturn
	 (setf (node-reoptimize node) t)
	 (ir1-optimize-return node))
	(mv-combination
	 (ir1-optimize-mv-combination node))
	(exit
	 (let ((value (exit-value node)))
	   (when value
	     (derive-node-type node (continuation-derived-type value)))))
	(cset
	 (ir1-optimize-set node)))))
  (undefined-value))

;;; Join-Successor-If-Possible  --  Internal
;;;
;;; We cannot combine with a successor block if:
;;;  1] The successor has more than one predecessor.
;;;  2] The last node's Cont is also used somewhere else.
;;;  3] The successor is the current block (looping forever).
;;;  4] The next block has a different cleanup, and thus we may want to insert
;;;     cleanup code between the two blocks at some point.
;;;  5] The next block has a different home lambda, and thus the control
;;;     transfer is a non-local exit.
;;;
;;; If we succeed, we return true, otherwise false.
;;;
;;; Joining is easy when the successor's Start continuation is the same
;;; from our Last's Cont.  If they differ, then we can still join when the
;;; last continuation has no next and the next continuation has no uses.
;;; In this case, we replace the next continuation with the last before
;;; joining the blocks.
;;;
(defun join-successor-if-possible (block)
  (declare (type cblock block))
  (let ((next (first (block-succ block))))
    (when (block-start next)
      (let* ((last (block-last block))
	     (last-cont (node-cont last))
	     (next-cont (block-start next)))
	(cond ((or (rest (block-pred next))
		   (not (eq (continuation-use last-cont) last))
		   (eq next block)
		   (not (eq (block-end-cleanup block)
			    (block-start-cleanup next)))
		   (not (eq (block-home-lambda block)
			    (block-home-lambda next))))
	       nil)
	      ((eq last-cont next-cont)
	       (join-blocks block next)
	       t)
	      ((and (null (block-start-uses next))
		    (eq (continuation-kind last-cont) :inside-block))
	       (let ((next-node (continuation-next next-cont)))
		 ;;
		 ;; If next-cont does have a dest, it must be unreachable,
		 ;; since there are no uses.  DELETE-CONTINUATION will mark the
		 ;; dest block as delete-p [and also this block, unless it is
		 ;; no longer backward reachable from the dest block.]
		 (delete-continuation next-cont)
		 (setf (node-prev next-node) last-cont)
		 (setf (continuation-next last-cont) next-node)
		 (setf (block-start next) last-cont)
		 (join-blocks block next))
	       t)
	      (t
	       nil))))))

;;; Join-Blocks  --  Internal
;;;
;;; Join together two blocks which have the same ending/starting
;;; continuation.  The code in Block2 is moved into Block1 and Block2 is
;;; deleted from the DFO.  We combine the optimize flags for the two blocks
;;; so that any indicated optimization gets done.
;;;
(defun join-blocks (block1 block2)
  (declare (type cblock block1 block2))
  (let* ((last (block-last block2))
	 (last-cont (node-cont last))
	 (succ (block-succ block2))
	 (start2 (block-start block2)))
    (do ((cont start2 (node-cont (continuation-next cont))))
	((eq cont last-cont)
	 (when (eq (continuation-kind last-cont) :inside-block)
	   (setf (continuation-block last-cont) block1)))
      (setf (continuation-block cont) block1))

    (unlink-blocks block1 block2)
    (dolist (block succ)
      (unlink-blocks block2 block)
      (link-blocks block1 block))

    (setf (block-last block1) last)
    (setf (continuation-kind start2) :inside-block))

  (setf (block-flags block1)
	(attributes-union (block-flags block1)
			  (block-flags block2)
			  (block-attributes type-asserted test-modified)))

  (let ((next (block-next block2))
	(prev (block-prev block2)))
    (setf (block-next prev) next)
    (setf (block-prev next) prev))

  (undefined-value))

;;; Flush-Dead-Code  --  Internal
;;;
;;; Delete any nodes in Block whose value is unused and have no
;;; side-effects.  We can delete sets of lexical variables when the set
;;; variable has no references.
;;;
;;; [### For now, don't delete potentially flushable calls when they have the
;;; Call attribute.  Someday we should look at the funcitonal args to determine
;;; if they have any side-effects.]
;;;
(defun flush-dead-code (block)
  (declare (type cblock block))
  (do-nodes-backwards (node cont block)
    (unless (continuation-dest cont)
      (typecase node
	(ref
	 (delete-ref node)
	 (unlink-node node))
	(combination
	 (let ((info (combination-kind node)))
	   (when (function-info-p info)
	     (let ((attr (function-info-attributes info)))
	       (when (and (ir1-attributep attr flushable)
			  (not (ir1-attributep attr call)))
		 (flush-dest (combination-fun node))
		 (dolist (arg (combination-args node))
		   (flush-dest arg))
		 (unlink-node node))))))
	(mv-combination
	 (when (eq (basic-combination-kind node) :local)
	   (let ((fun (combination-lambda node)))
	     (when (dolist (var (lambda-vars fun) t)
		     (when (or (leaf-refs var)
			       (lambda-var-sets var))
		       (return nil)))
	       (flush-dest (first (basic-combination-args node)))
	       (delete-let fun)))))
	(exit
	 (let ((value (exit-value node)))
	   (when value
	     (flush-dest value)
	     (setf (exit-value node) nil))))
	(cset
	 (let ((var (set-var node)))
	   (when (and (lambda-var-p var)
		      (null (leaf-refs var)))
	     (flush-dest (set-value node))
	     (setf (basic-var-sets var)
		   (delete node (basic-var-sets var)))
	     (unlink-node node)))))))

  (setf (block-flush-p block) nil)
  (undefined-value))

(declaim (end-block))


;;;; Local call return type propagation.

;;; Find-Result-Type  --  Internal
;;;
;;; This function is called on RETURN nodes that have their REOPTIMIZE flag
;;; set.  It iterates over the uses of the RESULT, looking for interesting
;;; stuff to update the TAIL-SET.  If a use isn't a local call, then we
;;; union its type together with the types of other such uses.  We assign
;;; to the RETURN-RESULT-TYPE the intersection of this type with the
;;; RESULT's asserted type.  We can make this intersection now (potentially
;;; before type checking) because this assertion on the result will
;;; eventually be checked (if appropriate.)
;;;
;;; We call MAYBE-CONVERT-TAIL-LOCAL-CALL on each local non-MV combination,
;;; which may change the succesor of the call to be the called function,
;;; and if so, checks if the call can become an assignment.  If we convert
;;; to an assignment, we abort, since the RETURN has been deleted.
;;;
(defun find-result-type (node)
  (declare (type creturn node))
  (let ((result (return-result node)))
    (collect ((use-union *empty-type* values-type-union))
      (do-uses (use result)
	(cond ((and (basic-combination-p use)
		    (eq (basic-combination-kind use) :local))
	       (assert (eq (lambda-tail-set (node-home-lambda use))
			   (lambda-tail-set (combination-lambda use))))
	       (when (combination-p use)
		 (when (nth-value 1 (maybe-convert-tail-local-call use))
		   (return-from find-result-type (undefined-value)))))
	      (t
	       (use-union (node-derived-type use)))))
      (let ((int (values-type-intersection (continuation-asserted-type result)
					   (use-union))))
	(setf (return-result-type node) int))))
  (undefined-value))

;;; IR1-Optimize-Return  --  Internal
;;;
;;; Do stuff to realize that something has changed about the value
;;; delivered to a return node.  Since we consider the return values of all
;;; functions in the tail set to be equivalent, this amounts to bringing
;;; the entire tail set up to date.  We iterate over the returns for all
;;; the functions in the tail set, reanalyzing them all (not treating Node
;;; specially.)
;;;
;;; When we are done, we check if the new type is different from the old
;;; TAIL-SET-TYPE.  If so, we set the type and also reoptimize all the
;;; continuations for references to functions in the tail set.  This will
;;; cause IR1-OPTIMIZE-COMBINATION to derive the new type as the results of
;;; the calls.
;;;
(defun ir1-optimize-return (node)
  (declare (type creturn node))
  (let* ((tails (lambda-tail-set (return-lambda node)))
	 (funs (tail-set-functions tails)))
    (collect ((res *empty-type* values-type-union))
      (dolist (fun funs)
	(let ((return (lambda-return fun)))
	  (when return
	    (when (node-reoptimize return)
	      (setf (node-reoptimize return) nil)
	      (find-result-type return))
	    (res (return-result-type return)))))

      (when (type/= (res) (tail-set-type tails))
	(setf (tail-set-type tails) (res))
	(dolist (fun (tail-set-functions tails))
	  (dolist (ref (leaf-refs fun))
	    (reoptimize-continuation (node-cont ref)))))))

  (undefined-value))


;;; IF optimization.

(declaim (start-block ir1-optimize-if))

;;; IR1-Optimize-If  --  Internal
;;;
;;; If the test has multiple uses, replicate the node when possible.  Also
;;; check if the predicate is known to be true or false, deleting the IF
;;; node in favor of the appropriate branch when this is the case.
;;;
(defun ir1-optimize-if (node)
  (declare (type cif node))
  (let ((test (if-test node))
	(block (node-block node)))

    (when (and (eq (block-start block) test)
	       (eq (continuation-next test) node)
	       (rest (block-start-uses block)))
      (do-uses (use test)
	(when (immediately-used-p test use)
	  (convert-if-if use node)
	  (when (continuation-use test) (return)))))

    (let* ((type (continuation-type test))
	   (victim
	    (cond ((constant-continuation-p test)
		   (if (continuation-value test)
		       (if-alternative node)
		       (if-consequent node)))
		  ((not (types-intersect type *null-type*))
		   (if-alternative node))
		  ((type= type *null-type*)
		   (if-consequent node)))))
      (when victim
	(flush-dest test)
	(when (rest (block-succ block))
	  (unlink-blocks block victim))
	(setf (component-reanalyze (block-component (node-block node))) t)
	(unlink-node node))))
  (undefined-value))

;;; Convert-If-If  --  Internal
;;;
;;; Create a new copy of an IF Node that tests the value of the node Use.
;;; The test must have >1 use, and must be immediately used by Use.  Node
;;; must be the only node in its block (implying that block-start =
;;; if-test).
;;;
;;; This optimization has an effect semantically similar to the
;;; source-to-source transformation:
;;;    (IF (IF A B C) D E) ==>
;;;    (IF A (IF B D E) (IF C D E))
;;;
;;; We clobber the NODE-SOURCE-PATH of both the original and the new node
;;; so that dead code deletion notes will definitely not consider either
;;; node to be part of the original source.  One node might become
;;; unreachable, resulting in a spurious note.
;;;
(defun convert-if-if (use node)
  (declare (type node use) (type cif node))
  (with-ir1-environment node
    (let* ((block (node-block node))
	   (test (if-test node))
	   (cblock (if-consequent node))
	   (ablock (if-alternative node))
	   (use-block (node-block use))
	   (dummy-cont (make-continuation))
	   (new-cont (make-continuation))
	   (new-node (make-if :test new-cont
			      :consequent cblock  :alternative ablock))
	   (new-block (continuation-starts-block new-cont)))
      (prev-link new-node new-cont)
      (setf (continuation-dest new-cont) new-node)
      (add-continuation-use new-node dummy-cont)
      (setf (block-last new-block) new-node)

      (unlink-blocks use-block block)
      (delete-continuation-use use)
      (add-continuation-use use new-cont)
      (link-blocks use-block new-block)

      (link-blocks new-block cblock)
      (link-blocks new-block ablock)

      (push "<IF Duplication>" (node-source-path node))
      (push "<IF Duplication>" (node-source-path new-node))

      (reoptimize-continuation test)
      (reoptimize-continuation new-cont)
      (setf (component-reanalyze *current-component*) t)))
  (undefined-value))

(declaim (end-block))


;;;; Exit IR1 optimization.

;;; Maybe-Delete-Exit  --  Interface
;;;
;;; This function attempts to delete an exit node, returning true if it
;;; deletes the block as a consequence:
;;; -- If the exit is degenerate (has no Entry), then we don't do anything,
;;;    since there is nothing to be done.
;;; -- If the exit node and its Entry have the same home lambda then we know
;;;    the exit is local, and can delete the exit.  We change uses of the
;;;    Exit-Value to be uses of the original continuation, then unlink the
;;;    node.  If the exit is to a TR context, then we must do MERGE-TAIL-SETS
;;;    on any local calls which delivered their value to this exit.
;;; -- If there is no value (as in a GO), then we skip the value semantics.
;;;
;;; This function is also called by environment analysis, since it wants all
;;; exits to be optimized even if normal optimization was omitted.
;;;
(defun maybe-delete-exit (node)
  (declare (type exit node))
  (let ((value (exit-value node))
	(entry (exit-entry node))
	(cont (node-cont node)))
    (when (and entry
	       (eq (node-home-lambda node) (node-home-lambda entry)))
      (setf (entry-exits entry) (delete node (entry-exits entry)))
      (prog1
	  (unlink-node node)
	(when value
	  (collect ((merges))
	    (when (return-p (continuation-dest cont))
	      (do-uses (use value)
		(when (and (basic-combination-p use)
			   (eq (basic-combination-kind use) :local))
		  (merges use))))
	    (substitute-continuation-uses cont value)
	    (dolist (merge (merges))
	      (merge-tail-sets merge))))))))


;;;; Combination IR1 optimization.

(declaim (start-block ir1-optimize-combination maybe-terminate-block
		      validate-call-type))

;;; Ir1-Optimize-Combination  --  Internal
;;;
;;; Do IR1 optimizations on a Combination node.
;;;
(defun ir1-optimize-combination (node)
  (declare (type combination node))
  (when (continuation-reoptimize (basic-combination-fun node))
    (propagate-function-change node))
  (let ((args (basic-combination-args node))
	(kind (basic-combination-kind node)))
    (case kind
      (:local
       (let ((fun (combination-lambda node)))
	 (if (eq (functional-kind fun) :let)
	     (propagate-let-args node fun)
	     (propagate-local-call-args node fun))))
      ((:full :error)
       (dolist (arg args)
	 (when arg
	   (setf (continuation-reoptimize arg) nil))))
      (t
       (dolist (arg args)
	 (when arg
	   (setf (continuation-reoptimize arg) nil)))

       (let ((attr (function-info-attributes kind)))
	 (when (and (ir1-attributep attr foldable)
		    (not (ir1-attributep attr call))
		    (every #'constant-continuation-p args)
		    (continuation-dest (node-cont node)))
	   (constant-fold-call node)
	   (return-from ir1-optimize-combination)))

       (let ((fun (function-info-derive-type kind)))
	 (when fun
	   (let ((res (funcall fun node)))
	     (when res
	       (derive-node-type node res)
	       (maybe-terminate-block node nil)))))

       (let ((fun (function-info-optimizer kind)))
	 (unless (and fun (funcall fun node))
	   (dolist (x (function-info-transforms kind))
	     (unless (ir1-transform node x)
	       (return))))))))

  (undefined-value))

;;; MAYBE-TERMINATE-BLOCK  --  Interface
;;;
;;; If Call is to a function that doesn't return (type NIL), then terminate
;;; the block there, and link it to the component tail.  We also change the
;;; call's CONT to be a dummy continuation to prevent the use from
;;; confusing things.
;;;
;;; Except when called during IR1, we delete the continuation if it has no
;;; other uses.  (If it does have other uses, we reoptimize.)
;;;
;;; Termination on the basis of a continuation type assertion is inhibited
;;; when:
;;; -- The continuation is deleted (hence the assertion is spurious), or
;;; -- We are in IR1 conversion (where THE assertions are subject to
;;;    weakening.)
;;;
(defun maybe-terminate-block (call ir1-p)
  (declare (type basic-combination call))
  (let* ((block (node-block call))
	 (cont (node-cont call))
	 (tail (component-tail (block-component block)))
	 (succ (first (block-succ block))))
    (unless (or (and (eq call (block-last block)) (eq succ tail))
		(block-delete-p block)
		*converting-for-interpreter*)
      (when (or (and (eq (continuation-asserted-type cont) *empty-type*)
		     (not (or ir1-p (eq (continuation-kind cont) :deleted))))
		(eq (node-derived-type call) *empty-type*))
	(cond (ir1-p
	       (delete-continuation-use call)
	       (cond
		((block-last block)
		 (assert (and (eq (block-last block) call)
			      (eq (continuation-kind cont) :block-start))))
		(t
		 (setf (block-last block) call)
		 (link-blocks block (continuation-starts-block cont)))))
	      (t
	       (node-ends-block call)
	       (delete-continuation-use call)
	       (if (eq (continuation-kind cont) :unused)
		   (delete-continuation cont)
		   (reoptimize-continuation cont))))

	(unlink-blocks block (first (block-succ block)))
	(setf (component-reanalyze (block-component block)) t)
	(assert (not (block-succ block)))
	(link-blocks block tail)
	(add-continuation-use call (make-continuation))
	t))))

;;; Recognize-Known-Call  --  Interface
;;;
;;; Called both by IR1 conversion and IR1 optimization when they have
;;; verified the type signature for the call, and are wondering if
;;; something should be done to special-case the call.  If Call is a call
;;; to a global function, then see if it defined or known:
;;; -- If a DEFINED-FUNCTION should be inline expanded, then convert the
;;;    expansion and change the call to call it.  Expansion is enabled if
;;;    :INLINE or if space=0.  If the FUNCTIONAL slot is true, we never expand,
;;;    since this function has already been converted.  Local call analysis
;;;    will duplicate the definition if necessary.  We claim that the parent
;;;    form is LABELS for context declarations, since we don't want it to be
;;;    considered a real global function.
;;; -- In addition to a direct check for the function name in the table, we
;;;    also must check for slot accessors.  If the function is a slot accessor,
;;;    then we set the combination kind to the function info of %Slot-Setter or
;;;    %Slot-Accessor, as appropriate.
;;; -- If it is a known function, mark it as such by setting the Kind.
;;;
;;; We return the leaf referenced (NIL if not a leaf) and the function-info
;;; assigned.
;;;
(defun recognize-known-call (call ir1-p)
  (declare (type combination call))
  (let* ((ref (continuation-use (basic-combination-fun call)))
	 (leaf (when (ref-p ref) (ref-leaf ref)))
	 (inlinep (if (and (defined-function-p leaf)
			   (not (byte-compiling)))
		      (defined-function-inlinep leaf)
		      :no-chance)))
    (cond
     ((eq inlinep :notinline) (values nil nil))
     ((not (and (global-var-p leaf)
		(eq (global-var-kind leaf) :global-function)))
      (values leaf nil))
     ((and (ecase inlinep
	     (:inline t)
	     (:no-chance nil)
	     ((nil :maybe-inline) (policy call (zerop space))))
	   (defined-function-inline-expansion leaf)
	   (let ((fun (defined-function-functional leaf)))
	     (or (not fun)
		 (and (eq inlinep :inline) (functional-kind fun))))
	   (inline-expansion-ok call))
      (flet ((frob ()
	       (let ((res (ir1-convert-lambda-for-defun
			   (defined-function-inline-expansion leaf)
			   leaf t
			   #'ir1-convert-inline-lambda
			   'labels)))
		 (setf (defined-function-functional leaf) res)
		 (change-ref-leaf ref res))))
	(if ir1-p
	    (frob)
	    (with-ir1-environment call
	      (frob)
	      (local-call-analyze *current-component*))))

      (values (ref-leaf (continuation-use (basic-combination-fun call)))
	      nil))
     (t
      (let* ((name (leaf-name leaf))
	     (info (info function info
			 (if (slot-accessor-p leaf)
			     (if (consp name)
				 '%slot-setter
				 '%slot-accessor)
			     name))))
	(if info
	    (values leaf (setf (basic-combination-kind call) info))
	    (values leaf nil)))))))

;;; VALIDATE-CALL-TYPE  --  Internal
;;;
;;; Check if Call satisfies Type.  If so, apply the type to the call, and
;;; do MAYBE-TERMINATE-BLOCK and return the values of RECOGNIZE-KNOWN-CALL.
;;; If an error, set the combination kind and return NIL, NIL.  If the type
;;; is just FUNCTION, then skip the syntax check, arg/result type
;;; processing, but still call RECOGNIZE-KNOWN-CALL, since the call might
;;; be to a known lambda, and that checking is done by local call analysis.
;;;
(defun validate-call-type (call type ir1-p)
  (declare (type combination call) (type ctype type))
  (cond ((not (function-type-p type))
	 (assert (multiple-value-bind
		     (val win)
		     (csubtypep type (specifier-type 'function))
		   (or val (not win))))
	 (recognize-known-call call ir1-p))
	((valid-function-use call type
			     :argument-test #'always-subtypep
			     :result-test #'always-subtypep
			     :error-function #'compiler-warning
			     :warning-function #'compiler-note)
	 (assert-call-type call type)
	 (maybe-terminate-block call ir1-p)
	 (recognize-known-call call ir1-p))
	(t
	 (setf (combination-kind call) :error)
	 (values nil nil))))

;;; Propagate-Function-Change  --  Internal
;;;
;;; Called by Ir1-Optimize when the function for a call has changed.  If
;;; the call is local, we try to let-convert it, and derive the result
;;; type.  If it is a :FULL call, we validate it against the type, which
;;; recognizes known calls, does inline expansion, etc.  If a call to a
;;; predicate in a non-conditional position or to a function with a source
;;; transform, then we reconvert the form to give IR1 another chance.
;;;
(defun propagate-function-change (call)
  (declare (type combination call))
  (let ((*compiler-error-context* call)
	(fun-cont (basic-combination-fun call)))
    (setf (continuation-reoptimize fun-cont) nil)
    (case (combination-kind call)
      (:local
       (let ((fun (combination-lambda call)))
	 (maybe-let-convert fun)
	 (unless (member (functional-kind fun) '(:let :assignment :deleted))
	   (derive-node-type call (tail-set-type (lambda-tail-set fun))))))
      (:full
       (multiple-value-bind
	   (leaf info)
	   (validate-call-type call (continuation-type fun-cont) nil)
	 (cond ((functional-p leaf)
		(convert-call-if-possible
		 (continuation-use (basic-combination-fun call))
		 call))
	       ((not leaf))
	       ((or (info function source-transform (leaf-name leaf))
		    (and info
			 (ir1-attributep (function-info-attributes info)
					 predicate)
			 (let ((dest (continuation-dest (node-cont call))))
			   (and dest (not (if-p dest))))))
		(let ((name (leaf-name leaf)))
		  (when (symbolp name)
		    (let ((dums (loop repeat (length (combination-args call))
				      collect (gensym))))
		      (transform-call call
				      `(lambda ,dums
					 (,name ,@dums))))))))))))
  (undefined-value))


;;;; Known function optimization.

;;; RECORD-OPTIMIZATION-FAILURE  --  Internal
;;;
;;; Add a failed optimization note to FAILED-OPTIMZATIONS for Node, Fun and
;;; Args.  If there is already a note for Node and Transform, replace it,
;;; otherwise add a new one.
;;;
(defun record-optimization-failure (node transform args)
  (declare (type combination node) (type transform transform)
	   (type (or function-type list) args))
  (let* ((table (component-failed-optimizations *compile-component*))
	 (found (assoc transform (gethash node table))))
    (if found
	(setf (cdr found) args)
	(push (cons transform args) (gethash node table))))
  (undefined-value))

;;; IR1-Transform  --  Internal
;;;
;;; Attempt to transform Node using Function, subject to the call type
;;; constraint Type.  If we are inhibited from doing the transform for some
;;; reason and Flame is true, then we make a note of the message in
;;; FAILED-OPTIMIZATIONS for IR1 finalize to pick up.  We return true if
;;; the transform failed, and thus further transformation should be
;;; attempted.  We return false if either the transform suceeded or was
;;; aborted.
;;;
(defun ir1-transform (node transform)
  (declare (type combination node) (type transform transform))
  (let* ((type (transform-type transform))
	 (fun (transform-function transform))
	 (constrained (function-type-p type))
	 (table (component-failed-optimizations *compile-component*))
	 (flame
	  (if (transform-important transform)
	      (policy node (>= speed brevity))
	      (policy node (> speed brevity))))
	 (*compiler-error-context* node))
    (cond ((let ((when (transform-when transform)))
	     (not (or (eq when :both)
		      (eq when (if *byte-compiling* :byte :native)))))
	   t)
	  ((or (not constrained)
	       (valid-function-use node type :strict-result t))
	   (multiple-value-bind
	       (severity args)
	       (catch 'give-up
		 (transform-call node (funcall fun node))
		 (values :none nil))
	     (ecase severity
	       (:none
		(remhash node table)
		nil)
	       (:aborted
		(setf (combination-kind node) :error)
		(when args
		  (apply #'compiler-warning args))
		(remhash node table)
		nil)
	       (:failure
		(if args
		    (when flame
		      (record-optimization-failure node transform args))
		    (setf (gethash node table)
			  (remove transform (gethash node table) :key #'car)))
		t)
	       (:delayed
		(remhash node table)
		nil))))
	  ((and flame
		(valid-function-use node type
				    :argument-test #'types-intersect
				    :result-test #'values-types-intersect))
	   (record-optimization-failure node transform type)
	   t)
	  (t
	   t))))

(declaim (end-block))

;;; give-up, abort-transform  --  Interface
;;;
;;; Just throw the severity and args...
;;;
(defun give-up (&rest args)
  "This function is used to throw out of an IR1 transform, aborting this
   attempt to transform the call, but admitting the possibility that this
   or some other transform will later suceed.  If arguments are supplied,
   they are format arguments for an efficiency note."
  (values nil)
  (throw 'give-up (values :failure args)))
;;;
(defun abort-transform (&rest args)
  "This function is used to throw out of an IR1 transform and force a
   normal call to the function at run time.  No further optimizations will
   be attempted."
  (throw 'give-up (values :aborted args)))

(defvar *delayed-transforms*)

;;; delay-transform  --  Interface
;;;
(defun delay-transform (node &rest reasons)
  "This function is used to throw out of an IR1 transform, and delay the
   transform on the node until later. The reasons specifies when the
   transform will be later retried. The :optimize reason causes the
   transform to be delayed until after the current IR1 optimization pass.
   The :constraint reason causes the transform to be delayed until after
   constraint propagation."
  (let ((assoc (assoc node *delayed-transforms*)))
    (cond ((not assoc)
	   (setf *delayed-transforms*
		 (acons node reasons *delayed-transforms*))
	   (throw 'give-up :delayed))
	  ((cdr assoc)
	   (dolist (reason reasons)
	     (pushnew reason (cdr assoc)))
	   (throw 'give-up :delayed)))))

;;; retry-delayed-transforms  --  Interface.
;;;
;;; Clear any delayed transform with no reasons - these should have been
;;; tried in the last pass. Then remove the reason from the delayed
;;; transform reasons, and if any become empty then set reoptimize flags
;;; for the node. Returns true if any transforms are to be retried.
;;;
(defun retry-delayed-transforms (reason)
  (setf *delayed-transforms* (keep-if #'cdr *delayed-transforms*))
  (let ((reoptimize nil))
    (dolist (assoc *delayed-transforms*)
      (let ((reasons (remove reason (cdr assoc))))
	(setf (cdr assoc) reasons)
	(unless reasons
	  (let ((node (car assoc)))
	    (unless (node-deleted node)
	      (setf reoptimize t)
	      (setf (node-reoptimize node) t)
	      (let ((block (node-block node)))
		(setf (block-reoptimize block) t)
		(setf (component-reoptimize (block-component block)) t)))))))
    reoptimize))

;;; Transform-Call  --  Internal
;;;
;;; Take the lambda-expression Res, IR1 convert it in the proper
;;; environment, and then install it as the function for the call Node.  We
;;; do local call analysis so that the new function is integrated into the
;;; control flow.
;;;
(defun transform-call (node res)
  (declare (type combination node) (list res))
  (with-ir1-environment node
    (let ((new-fun (ir1-convert-inline-lambda res))
	  (ref (continuation-use (combination-fun node))))
      (change-ref-leaf ref new-fun)
      (setf (combination-kind node) :full)
      (local-call-analyze *current-component*)))
  (undefined-value))

;;; Constant-Fold-Call  --  Internal
;;;
;;; Replace a call to a foldable function of constant arguments with the
;;; result of evaluating the form.  We insert the resulting constant node
;;; after the call, stealing the call's continuation.  We give the call a
;;; continuation with no Dest, which should cause it and its arguments to
;;; go away.  If there is an error during the evaluation, we give a warning
;;; and leave the call alone, making the call a :ERROR call.
;;;
;;; If there is more than one value, then we transform the call into a
;;; values form.
;;;
(defun constant-fold-call (call)
  (declare (type combination call))
  (let* ((args (mapcar #'continuation-value (combination-args call)))
	 (ref (continuation-use (combination-fun call)))
	 (fun (leaf-name (ref-leaf ref))))

    (multiple-value-bind (values win)
			 (careful-call fun args call "constant folding")
      (cond
       ((not win)
	(setf (combination-kind call) :error))
       ;; X Always transform the call below so that non-flushable
       ;; functions get flushed if the constant folding works.
       #+nil
       ((= (length values) 1)
	(with-ir1-environment call
	  (when (producing-fasl-file)
	    (maybe-emit-make-load-forms (first values)))
	  (let* ((leaf (find-constant (first values)))
		 (node (make-ref (leaf-type leaf) leaf))
		 (dummy (make-continuation))
		 (cont (node-cont call))
		 (block (node-block call))
		 (next (continuation-next cont)))
	    (push node (leaf-refs leaf))
	    (setf (leaf-ever-used leaf) t)

	    (delete-continuation-use call)
	    (add-continuation-use call dummy)
	    (prev-link node dummy)
	    (add-continuation-use node cont)
	    (setf (continuation-next cont) next)
	    (when (eq call (block-last block))
	      (setf (block-last block) node))
	    (reoptimize-continuation cont))))
       (t
	(let ((dummies (loop repeat (length args)
			     collect (gensym))))
	  (transform-call
	   call
	   `(lambda ,dummies
	      (declare (ignore ,@dummies))
	      (values ,@(mapcar #'(lambda (x) `',x) values)))))))))

  (undefined-value))


;;;; Local call optimization.

(declaim (start-block ir1-optimize-set constant-reference-p delete-let
		      propagate-let-args propagate-local-call-args
		      propagate-to-refs propagate-from-sets
		      ir1-optimize-mv-combination))

;;; Propagate-To-Refs  --  Internal
;;;
;;; Propagate Type to Leaf and its Refs, marking things changed.  If the
;;; leaf type is a function type, then just leave it alone, since TYPE is
;;; never going to be more specific than that (and TYPE-INTERSECTION would
;;; choke.)
;;;
(defun propagate-to-refs (leaf type)
  (declare (type leaf leaf) (type ctype type))
  (let ((var-type (leaf-type leaf)))
    (unless (function-type-p var-type)
      (let ((int (type-intersection var-type type)))
	(when (type/= int var-type)
	  (setf (leaf-type leaf) int)
	  (dolist (ref (leaf-refs leaf))
	    (derive-node-type ref int))))
      (undefined-value))))

;;; PROPAGATE-FROM-SETS  --  Internal
;;;
;;; Figure out the type of a LET variable that has sets.  We compute the
;;; union of the initial value Type and the types of all the set values and
;;; to a PROPAGATE-TO-REFS with this type.
;;;
(defun propagate-from-sets (var type)
  (collect ((res type type-union))
    (dolist (set (basic-var-sets var))
      (res (continuation-type (set-value set)))
      (setf (node-reoptimize set) nil))
    (propagate-to-refs var (res)))
  (undefined-value))

;;; IR1-OPTIMIZE-SET  --  Internal
;;;
;;; If a let variable, find the initial value's type and do
;;; PROPAGATE-FROM-SETS.  We also derive the VALUE's type as the node's
;;; type.
;;;
(defun ir1-optimize-set (node)
  (declare (type cset node))
  (let ((var (set-var node)))
    (when (and (lambda-var-p var) (leaf-refs var))
      (let ((home (lambda-var-home var)))
	(when (eq (functional-kind home) :let)
	  (let ((iv (let-var-initial-value var)))
	    (setf (continuation-reoptimize iv) nil)
	    (propagate-from-sets var (continuation-type iv)))))))

  (derive-node-type node (continuation-type (set-value node)))
  (undefined-value))

;;; CONSTANT-REFERENCE-P  --  Interface
;;;
;;; Return true if the value of Ref will always be the same (and is thus
;;; legal to substitute.)
;;;
(defun constant-reference-p (ref)
  (declare (type ref ref))
  (let ((leaf (ref-leaf ref)))
    (typecase leaf
      ((or constant functional) t)
      (lambda-var
       (null (lambda-var-sets leaf)))
      (defined-function
       (not (eq (defined-function-inlinep leaf) :notinline)))
      (global-var
       (case (global-var-kind leaf)
	 (:global-function t)
	 (:constant t))))))

;;; SUBSTITUTE-SINGLE-USE-CONTINUATION  --  Internal
;;;
;;; If we have a non-set let var with a single use, then (if possible)
;;; replace the variable reference's CONT with the arg continuation.  This
;;; is inhibited when:
;;; -- CONT has other uses, or
;;; -- CONT receives multiple values, or
;;; -- the reference is in a different environment from the variable, or
;;; -- either continuation has a funky TYPE-CHECK annotation.
;;; -- the continuations have incompatible assertions, so the new asserted type
;;;    would be NIL.
;;; -- CONT's assertion is incompatbile with the proven type of ARG's, such as
;;;    when ARG returns multiple values and CONT has a single value assertion.
;;; -- the var's DEST has a different policy than the ARG's (think safety).
;;;
;;; We change the Ref to be a reference to NIL with unused value, and let
;;; it be flushed as dead code.  A side-effect of this substitution is to
;;; delete the variable.
;;;
(defun substitute-single-use-continuation (arg var)
  (declare (type continuation arg) (type lambda-var var))
  (let* ((ref (first (leaf-refs var)))
	 (cont (node-cont ref))
	 (cont-atype (continuation-asserted-type cont))
	 (dest (continuation-dest cont)))
    (when (and (eq (continuation-use cont) ref)
	       dest
	       (not (typep dest '(or creturn exit mv-combination)))
	       (eq (node-home-lambda ref)
		   (lambda-home (lambda-var-home var)))
	       (member (continuation-type-check arg) '(t nil))
	       (member (continuation-type-check cont) '(t nil))
	       (not (eq (values-type-intersection
			 cont-atype (continuation-asserted-type arg))
			*empty-type*))
	       (not (eq (values-type-intersection
			 cont-atype (continuation-proven-type arg))
			*empty-type*))
	       (eq (lexenv-cookie (node-lexenv dest))
		   (lexenv-cookie (node-lexenv (continuation-dest arg)))))
      (assert (member (continuation-kind arg)
		      '(:block-start :deleted-block-start :inside-block)))
      (assert-continuation-type arg cont-atype)
      (setf (node-derived-type ref) *wild-type*)
      (change-ref-leaf ref (find-constant nil))
      (substitute-continuation arg cont)
      (reoptimize-continuation arg)
      t)))

;;; DELETE-LET  --  Interface
;;;
;;; Delete a Let, removing the call and bind nodes, and warning about any
;;; unreferenced variables.  Note that FLUSH-DEAD-CODE will come along
;;; right away and delete the REF and then the lambda, since we flush the
;;; FUN continuation.
;;;
(defun delete-let (fun)
  (declare (type clambda fun))
  (assert (member (functional-kind fun) '(:let :mv-let)))
  (note-unreferenced-vars fun)
  (let ((call (let-combination fun)))
    (flush-dest (basic-combination-fun call))
    (unlink-node call)
    (unlink-node (lambda-bind fun))
    (setf (lambda-bind fun) nil))
  (undefined-value))

;;; Propagate-Let-Args  --  Internal
;;;
;;; This function is called when one of the arguments to a LET changes.  We
;;; look at each changed argument.  If the corresponding variable is set,
;;; then we call PROPAGATE-FROM-SETS.  Otherwise, we consider substituting
;;; for the variable, and also propagate derived-type information for the
;;; arg to all the Var's refs.
;;;
;;; Substitution is inhibited when the arg leaf's derived type isn't a
;;; subtype of the argument's asserted type.  This prevents type checking
;;; from being defeated, and also ensures that the best representation for
;;; the variable can be used.
;;;
;;; Substitution of individual references is inhibited if the reference is
;;; in a different component from the home.  This can only happen with
;;; closures over top-level lambda vars.  In such cases, the references may
;;; have already been compiled, and thus can't be retroactively modified.
;;;
;;; If all of the variables are deleted (have no references) when we are
;;; done, then we delete the let.
;;;
;;; Note that we are responsible for clearing the Continuation-Reoptimize
;;; flags.
;;;
(defun propagate-let-args (call fun)
  (declare (type combination call) (type clambda fun))
  (loop for arg in (combination-args call)
        and var in (lambda-vars fun) do
    (when (and arg (continuation-reoptimize arg))
      (setf (continuation-reoptimize arg) nil)
      (cond
       ((lambda-var-sets var)
	(propagate-from-sets var (continuation-type arg)))
       ((let ((use (continuation-use arg)))
	  (when (ref-p use)
	    (let ((leaf (ref-leaf use)))
	      (when (and (constant-reference-p use)
			 (values-subtypep (leaf-type leaf)
					  (continuation-asserted-type arg)))
		(propagate-to-refs var (continuation-type arg))
		(let ((this-comp (block-component (node-block use))))
		  (substitute-leaf-if
		   #'(lambda (ref)
		       (cond ((eq (block-component (node-block ref))
				  this-comp)
			      t)
			     (t
			      (assert (eq (functional-kind (lambda-home fun))
					  :top-level))
			      nil)))
		   leaf var))
		t)))))
       ((and (null (rest (leaf-refs var)))
	     (not *byte-compiling*)
	     (substitute-single-use-continuation arg var)))
       (t
	(propagate-to-refs var (continuation-type arg))))))

  (when (every #'null (combination-args call))
    (delete-let fun))

  (undefined-value))

;;; Propagate-Local-Call-Args  --  Internal
;;;
;;; This function is called when one of the args to a non-let local call
;;; changes.  For each changed argument corresponding to an unset variable,
;;; we compute the union of the types across all calls and propagate this
;;; type information to the var's refs.
;;;
;;; If the function has an XEP, then we don't do anything, since we won't
;;; discover anything.
;;;
;;; We can clear the Continuation-Reoptimize flags for arguments in all
;;; calls corresponding to changed arguments in Call, since the only use in
;;; IR1 optimization of the Reoptimize flag for local call args is right
;;; here.
;;;
(defun propagate-local-call-args (call fun)
  (declare (type combination call) (type clambda fun))

  (unless (or (functional-entry-function fun)
	      (lambda-optional-dispatch fun))
    (let* ((vars (lambda-vars fun))
	   (union (mapcar #'(lambda (arg var)
			      (when (and arg
					 (continuation-reoptimize arg)
					 (null (basic-var-sets var)))
				(continuation-type arg)))
			  (basic-combination-args call)
			  vars))
	   (this-ref (continuation-use (basic-combination-fun call))))

      (dolist (arg (basic-combination-args call))
	(when arg
	  (setf (continuation-reoptimize arg) nil)))

      (dolist (ref (leaf-refs fun))
	(let ((dest (continuation-dest (node-cont ref))))
	  (unless (or (eq ref this-ref) (not dest))
	    (setq union
		  (mapcar #'(lambda (this-arg old)
			      (when old
				(setf (continuation-reoptimize this-arg) nil)
				(type-union (continuation-type this-arg) old)))
			  (basic-combination-args dest)
			  union)))))

      (mapc #'(lambda (var type)
		(when type
		  (propagate-to-refs var type)))
	    vars union)))

  (undefined-value))

(declaim (end-block))


;;;; Multiple values optimization.

;;; IR1-OPTIMIZE-MV-COMBINATION  --  Internal
;;;
;;; Do stuff to notice a change to a MV combination node.  There are two
;;; main branches here:
;;;  -- If the call is local, then it is already a MV let, or should become one.
;;;     Note that although all :LOCAL MV calls must eventually be converted to
;;;     :MV-LETs, there can be a window when the call is local, but has not
;;;     been let converted yet.  This is because the entry-point lambdas may
;;;     have stray references (in other entry points) that have not been
;;;     deleted yet.
;;;  -- The call is full.  This case is somewhat similar to the non-MV
;;;     combination optimization: we propagate return type information and
;;;     notice non-returning calls.  We also have an optimization
;;;     which tries to convert MV-CALLs into MV-binds.
;;;
(defun ir1-optimize-mv-combination (node)
  (ecase (basic-combination-kind node)
    (:local
     (let ((fun-cont (basic-combination-fun node)))
       (when (continuation-reoptimize fun-cont)
	 (setf (continuation-reoptimize fun-cont) nil)
	 (maybe-let-convert (combination-lambda node))))
     (setf (continuation-reoptimize (first (basic-combination-args node))) nil)
     (when (eq (functional-kind (combination-lambda node)) :mv-let)
       (unless (convert-mv-bind-to-let node)
	 (ir1-optimize-mv-bind node))))
    (:full
     (let* ((fun (basic-combination-fun node))
	    (fun-changed (continuation-reoptimize fun))
	    (args (basic-combination-args node)))
       (when fun-changed
	 (setf (continuation-reoptimize fun) nil)
	 (let ((type (continuation-type fun)))
	   (when (function-type-p type)
	     (derive-node-type node (function-type-returns type))))
	 (maybe-terminate-block node nil)
	 (let ((use (continuation-use fun)))
	   (when (and (ref-p use) (functional-p (ref-leaf use)))
	     (convert-call-if-possible use node)
	     (when (eq (basic-combination-kind node) :local)
	       (maybe-let-convert (ref-leaf use))))))
       (unless (or (eq (basic-combination-kind node) :local)
		   (eq (continuation-function-name fun) '%throw))
	 (ir1-optimize-mv-call node))
       (dolist (arg args)
	 (setf (continuation-reoptimize arg) nil))))
    (:error))
  (undefined-value))

;;; Values-types-defaulted  --  Internal
;;;
;;; Like values-types, but returns the types of the given number of
;;; arguments. If optional of rest values must be used then the union with
;;; the null type is computed in case of defaulting, and if no values are
;;; available then they are defaulted to the null type.
;;;
(defun values-types-defaulted (type count)
  (declare (type ctype type) (type index count))
  (cond ((eq type *wild-type*)
	 (let ((types nil))
	   (dotimes (i count types)
	     (push *universal-type* types))))
	((not (values-type-p type))
	 (let ((types nil))
	   (dotimes (i (1- count))
	     (push *null-type* types))
	   (push type types)))
	(t
	 (let ((required (args-type-required type))
	       (optional (args-type-optional type))
	       (keyp-allowp (or (args-type-keyp type) (args-type-allowp type)))
	       (rest (args-type-rest type)))
	   (collect ((types))
	     (dotimes (i count)
	       (types (cond (required (single-value-type (pop required)))
			    (optional (values-type-union
				       (single-value-type (pop optional))
				       *null-type*))
			    (keyp-allowp *universal-type*)
			    (rest (values-type-union (single-value-type rest)
						     *null-type*))
			    (t *null-type*))))
	     (types))))))

;;; IR1-OPTIMIZE-MV-BIND  --  Internal
;;;
;;; Propagate derived type info from the values continuation to the vars.
;;;
(defun ir1-optimize-mv-bind (node)
  (declare (type mv-combination node))
  (let ((arg (first (basic-combination-args node)))
	(vars (lambda-vars (combination-lambda node))))
    (let ((types (values-types-defaulted (continuation-derived-type arg)
					 (length vars))))
      (mapc #'(lambda (var type)
		(if (basic-var-sets var)
		    (propagate-from-sets var type)
		    (propagate-to-refs var type)))
	    vars types))

    (setf (continuation-reoptimize arg) nil))
  (undefined-value))

;;; IR1-OPTIMIZE-MV-CALL  --  Internal
;;;
;;; If possible, convert a general MV call to an MV-BIND.  We can do this
;;; if:
;;; -- The call has only one argument, and
;;; -- The function has a known fixed number of arguments, or
;;; -- The argument yields a known fixed number of values.
;;;
;;; What we do is change the function in the MV-CALL to be a lambda that "looks
;;; like an MV bind", which allows IR1-OPTIMIZE-MV-COMBINATION to notice that
;;; this call can be converted (the next time around.)  This new lambda just
;;; calls the actual function with the MV-BIND variables as arguments.  Note
;;; that this new MV bind is not let-converted immediately, as there are going
;;; to be stray references from the entry-point functions until they get
;;; deleted.
;;;
;;; In order to avoid loss of argument count checking, we only do the
;;; transformation according to a known number of expected argument if safety
;;; is unimportant.  We can always convert if we know the number of actual
;;; values, since the normal call that we build will still do any appropriate
;;; argument count checking.
;;;
;;; We only attempt the transformation if the called function is a constant
;;; reference.  This allows us to just splice the leaf into the new function,
;;; instead of trying to somehow bind the function expression.  The leaf must
;;; be constant because we are evaluating it again in a different place.  This
;;; also has the effect of squelching multiple warnings when there is an
;;; argument count error.
;;;
(defun ir1-optimize-mv-call (node)
  (let ((fun (basic-combination-fun node))
	(*compiler-error-context* node)
	(ref (continuation-use (basic-combination-fun node)))
	(args (basic-combination-args node)))

    (unless (and (ref-p ref) (constant-reference-p ref)
		 args (null (rest args)))
      (return-from ir1-optimize-mv-call))

    (multiple-value-bind (min max)
			 (function-type-nargs (continuation-type fun))
      (let ((total-nvals
	     (multiple-value-bind
		 (types nvals)
		 (values-types (continuation-derived-type (first args)))
	       (declare (ignore types))
	       (if (eq nvals :unknown) nil nvals))))

	(when total-nvals
	  (when (and min (< total-nvals min))
	    (compiler-warning
	     "MULTIPLE-VALUE-CALL with ~R values when the function expects ~
	     at least ~R."
	     total-nvals min)
	    (setf (basic-combination-kind node) :error)
	    (return-from ir1-optimize-mv-call))
	  (when (and max (> total-nvals max))
	    (compiler-warning
	     "MULTIPLE-VALUE-CALL with ~R values when the function expects ~
	     at most ~R."
	     total-nvals max)
	    (setf (basic-combination-kind node) :error)
	    (return-from ir1-optimize-mv-call)))

	(let ((count (cond (total-nvals)
			   ((and (policy node (zerop safety)) (eql min max))
			    min)
			   (t nil))))
	  (when count
	    (with-ir1-environment node
	      (let* ((dums (loop repeat count collect (gensym)))
		     (ignore (gensym))
		     (fun (ir1-convert-lambda
			   `(lambda (&optional ,@dums &rest ,ignore)
			      (declare (ignore ,ignore))
			      (funcall ,(ref-leaf ref) ,@dums)))))
		(change-ref-leaf ref fun)
		(assert (eq (basic-combination-kind node) :full))
		(local-call-analyze *current-component*)
		(assert (eq (basic-combination-kind node) :local)))))))))
  (undefined-value))

;;; CONVERT-MV-BIND-TO-LET  --  Internal
;;;
;;; If we see:
;;;    (multiple-value-bind (x y)
;;;                         (values xx yy)
;;;      ...)
;;; Convert to:
;;;    (let ((x xx)
;;;          (y yy))
;;;      ...)
;;;
;;; What we actually do is convert the VALUES combination into a normal let
;;; combination calling the original :MV-LET lambda.  If there are extra args to
;;; VALUES, discard the corresponding continuations.  If there are insufficient
;;; args, insert references to NIL.
;;;
(defun convert-mv-bind-to-let (call)
  (declare (type mv-combination call))
  (let* ((arg (first (basic-combination-args call)))
	 (use (continuation-use arg)))
    (when (and (combination-p use)
	       (eq (continuation-function-name (combination-fun use))
		   'values))
      (let* ((fun (combination-lambda call))
	     (vars (lambda-vars fun))
	     (vals (combination-args use))
	     (nvars (length vars))
	     (nvals (length vals)))
	(cond ((> nvals nvars)
	       (mapc #'flush-dest (subseq vals nvars))
	       (setq vals (subseq vals 0 nvars)))
	      ((< nvals nvars)
	       (with-ir1-environment use
		 (let ((node-prev (node-prev use)))
		   (setf (node-prev use) nil)
		   (setf (continuation-next node-prev) nil)
		   (collect ((res vals))
		     (loop as cont = (make-continuation use)
			   and prev = node-prev then cont
			   repeat (- nvars nvals)
			   do (reference-constant prev cont nil)
			      (res cont))
		     (setq vals (res)))
		   (prev-link use (car (last vals)))))))
	(setf (combination-args use) vals)
	(flush-dest (combination-fun use))
	(let ((fun-cont (basic-combination-fun call)))
	  (setf (continuation-dest fun-cont) use)
	  (setf (combination-fun use) fun-cont))
	(setf (combination-kind use) :local)
	(setf (functional-kind fun) :let)
	(flush-dest (first (basic-combination-args call)))
	(unlink-node call)
	(when vals
	  (reoptimize-continuation (first vals)))
	(propagate-to-args use fun))
      t)))


;;; VALUES-LIST IR1 optimizer  --  Internal
;;;
;;; If we see:
;;;    (values-list (list x y z))
;;;
;;; Convert to:
;;;    (values x y z)
;;;
;;; In implementation, this is somewhat similar to CONVERT-MV-BIND-TO-LET.  We
;;; grab the args of LIST and make them args of the VALUES-LIST call, flushing
;;; the old argument continuation (allowing the LIST to be flushed.)
;;;
(defoptimizer (values-list optimizer) ((list) node)
  (let ((use (continuation-use list)))
    (when (and (combination-p use)
	       (eq (continuation-function-name (combination-fun use))
		   'list))
      (change-ref-leaf (continuation-use (combination-fun node))
		       (find-free-function 'values "in a strange place"))
      (setf (combination-kind node) :full)
      (let ((args (combination-args use)))
	(dolist (arg args)
	  (setf (continuation-dest arg) node))
	(setf (combination-args use) nil)
	(flush-dest list)
	(setf (combination-args node) args))
      t)))


;;; VALUES IR1 transform  --  Internal
;;;
;;; If VALUES appears in a non-MV context, then effectively convert it to a
;;; PROG1.  This allows the computation of the additional values to become
;;; dead code.  Some attempt is made to correct the node derived type,
;;; setting it to the received single-value-type. The node continuation
;;; asserted type must also be adjusted, taking care when the continuation
;;; has multiple uses.
;;;
(deftransform values ((&rest vals) * * :node node)
  (let ((cont (node-cont node)))
    (when (typep (continuation-dest cont) '(or creturn exit mv-combination))
      (give-up))
    (flet ((first-value-type (type)
	     (declare (type ctype type))
	     (cond ((values-type-p type)
		    (let ((required (args-type-required type)))
		      (if required
			  (first required)
			  (let ((otype (args-type-optional type)))
			    (cond (otype (first otype))
				  ((or (args-type-keyp type)
				       (args-type-allowp type))
				   *universal-type*)
				  ((args-type-rest type))
				  (t *null-type*))))))
		   ((eq type *wild-type*)
		    *universal-type*)
		   (t
		    type))))
      (cond ((= (length (find-uses cont)) 1)
	     (setf (node-derived-type node)
		   (single-value-type (node-derived-type node)))
	     (setf (continuation-asserted-type cont)
		   (first-value-type (continuation-asserted-type cont))))
	    (t
	     (setf (node-derived-type node)
		   (single-value-type (node-derived-type node)))
	     (setf (continuation-asserted-type cont)
		   (values-type-union (continuation-asserted-type cont)
				      (first-value-type
				       (continuation-asserted-type cont)))))))
    (reoptimize-continuation cont)
    (if vals
	(let ((dummies (loop repeat (1- (length vals))
			     collect (gensym))))
	  `(lambda (val ,@dummies)
	     (declare (ignore ,@dummies))
	     val))
	'nil)))
