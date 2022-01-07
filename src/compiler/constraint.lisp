;;; The constraint propagation phase of the compiler, which uses global
;;; flow analysis to obtain dynamic type information.

(in-package "C")

#[ Type Propagation

    Use global flow analysis to propagate information about lexical
    variable types.  Eliminate redundant type checks and tests.

Phase position: 5/23 (front)

Presence: optional

Files: constraint

Entry functions: `constraint-propagate'

Call sequences:

    constraint-propagate
      init-var-constraints
      find-test-constraints
        add-test-constraint
      find-block-type-constraints
        find-constraint
      flow-propagate-constraints
      use-result-constraints
        constrain-ref-type
          change-ref-leaf
          derive-node-type

Uses the sparse set interface provided by sset.lisp.


FIX is there an intro to the types?
        ctype, included in structures like numeric-type
            class-info slot with type-class
        constraints are recorded per block, in sets
            "lexical variable types" (vs node,cont types? ~ vs closed over (specials?))
            do these originate at assertions?
        lambda-var constraints record all constrs for a var
            init'd here by init-var-constraints
            added by find-constraint when first added to block
        node has derived-type
            the type of the node
            set with derive-node-type
                via constraint-propagate (constrain-ref-type)
                      intersection,combination of block constrs and lambda-var constrs
                and in ir1opt (eg ir1-optimize-set)
        cont has  asserted type  from set-variable via setq, and others
                  proven type    basically node derived type
                  derived-type   combination of proven,asserted (cached in slot)
        where does declare fit into this?

New lambda-var-slot:

constraints: a list of all the constraints on this var for either X or Y.

    How to maintain consistency?  Does it really matter if there are constraints
    with deleted vars lying around?  Note that whatever mechanism we use for
    getting the constraints in the first place should tend to keep them up to date.
    Probably we would define optimizers for the interesting relations that look at
    their CONT's dest and annotate it if it is an IF.

    But maybe it is more trouble than it is worth trying to build up the set of
    constraints during ICR optimize (maintaining consistency in the process).
    Since ICR optimize iterates a bunch of times before it converges, we would be
    wasting time recomputing the constraints, when nobody uses them till constraint
    propagation runs.

    It seems that the only possible win is if we re-ran constraint propagation
    (which we might want to do.)  In that case, we wouldn't have to recompute all
    the constraints from scratch.  But it seems that we could do this just as well
    by having ICR optimize invalidate the affected parts of the constraint
    annotation, rather than trying to keep them up to date.  This also fits better
    with the optional nature of constraint propagation, since we don't want ICR
    optimize to commit to doing a lot of the work of constraint propagation.

    For example, we might have a per-block flag indicating that something happened
    in that block since the last time constraint propagation ran.  We might have
    different flags to represent the distinction between discovering a new type
    assertion inside the block and discovering something new about an if
    predicate, since the latter would be cheaper to update and probably is more
    common.

    It's fairly easy to see how we can build these sets of restrictions and
    propagate them using flow analysis, but actually using this information seems
    a bit more ad-hoc.

    Probably the biggest thing we do is look at all the refs.  If have proven that
    the value is EQ (EQL for a number) to some other leaf (constant or lambda-var),
    then we can substitute for that reference.  In some cases, we will want to do
    special stuff depending on the DEST.  If the dest is an IF and we proved (not
    null), then we can substitute T.  And if the dest is some relation on the same
    two lambda-vars, then we want to see if we can show that relation is definitely
    true or false.

    Otherwise, we can do our best to invert the set of restrictions into a type.
    Since types hold only constant info, we have to ignore any constraints between
    two vars.  We can make some use of negated type restrictions by using
    TYPE-DIFFERENCE to remove the type from the ref types.  If our inferred type is
    as good as the type assertion, then the continuation's type-check flag will be
    cleared.

    It really isn't much of a problem that we don't infer union types on joins,
    since union types are relatively easy to derive without using flow information.
    The normal bottom-up type inference done by ICR optimize does this for us: it
    annotates everything with the union of all of the things it might possibly be.
    Then constraint propagation subtracts out those types that can't be in effect
    because of predicates or checks.



This phase is optional, but is desirable if anything is more important than
compilation speed.  We use an algorithm similar to available expressions to
propagate variable type information that has been discovered by implicit or
explicit type tests, or by type inference.

We must do a pre-pass which locates set closure variables, since we cannot do
flow analysis on such variables.  We set a flag in each set closure variable so
that we can quickly tell that it is losing when we see it again.  Although this
may seem to be wastefully redundant with environment analysis, the overlap
isn't really that great, and the cost should be small compared to that of the
flow analysis that we are preparing to do.

    Or we could punt on set variables...

A type constraint is a structure that includes sset-element and has the type
and variable.
    XXX Also a not-p flag indicating whether the sense is negated.
  Each variable has a list of its type constraints.  We create a
type constraint when we see a type test or check.  If there is already a
constraint for the same variable and type, then we just re-use it.  If there is
already a weaker constraint, then we generate both the weak constraints and the
strong constraint so that the weak constraints won't be lost even if the strong
one is unavailable.

We find all the distinct type constraints for each variable during the pre-pass
over the lambda nesting.  Each constraint has a list of the weaker constraints
so that we can easily generate them.

Every block generates all the type constraints in it, but a constraint is
available in a successor only if it is available in all predecessors.  We
determine the actual type constraint for a variable at a block by intersecting
all the available type constraints for that variable.

This isn't maximally tense when there are constraints that are not
hierarchically related, e.g. (or a b) (or b c).  If these constraints were
available from two predecessors, then we could infer that we have an (or a b c)
constraint, but the above algorithm would come up with none.  This probably
isn't a big problem.

    XXX Do we want to deal with (if (eq <var> '<foo>) ...) indicating singleton
        member type?

We detect explicit type tests by looking at type test annotation in the IF
node.  If there is a type check, the OUT sets are stored in the node, with
different sets for the consequent and alternative.  Implicit type checks are
located by finding Ref nodes whose Cont has the Type-Check flag set.  We don't
actually represent the GEN sets, we just initialize OUT to it, and then form
the union in place.

When we do the post-pass, we clear the Type-Check flags in the continuations
for Refs when we discover that the available constraints satisfy the asserted
type.  Any explicit uses of typep should be cleaned up by the ICR optimizer for
typep.  We can also set the derived type for Refs to the intersection of the
available type assertions.  If we discover anything, we should consider redoing
ICR optimization, since better type information might enable more
optimizations.
]#

(defstruct (constraint
	    (:include sset-element)
	    (:constructor make-constraint (number kind x y not-p)))
  ;;
  ;; The kind of constraint we have:
  ;;
  ;; TYPEP
  ;;     X is a LAMBDA-VAR and Y is a CTYPE.  The value of X is constrained to
  ;;     be of type Y.
  ;;
  ;; >, <
  ;;     X is a lambda-var and Y is a CTYPE.  The relation holds between X and
  ;;     some object of type Y.
  ;;
  ;; EQL
  ;;     X is a LAMBDA-VAR and Y is a LAMBDA-VAR or a CONSTANT.  The
  ;;     relation is asserted to hold.
  ;;
  (kind nil :type (member typep < > eql))
  ;;
  ;; The operands to the relation.
  (x nil :type lambda-var)
  (y nil :type (or ctype lambda-var constant))
  ;;
  ;; If true, negates the sense of the constraint.  The relation does *not*
  ;; hold.  (FIX Does that mean the inverse of the relation holds?)
  (not-p nil :type boolean))

(defvar *constraint-number*)

;;; FIND-CONSTRAINT  --  Interface
;;;
;;; Return a constraint for the specified arguments.  We only create a new
;;; constraint if there isn't already an equivalent old one, guaranteeing
;;; that all equivalent constraints are EQ.  This shouldn't be called on
;;; lambda-vars with no CONSTRAINTS set.
;;;
(defun find-constraint (kind x y not-p)
  (declare (type lambda-var x) (type (or constant lambda-var ctype) y)
	   (type boolean not-p))
  (or (etypecase y
	(ctype
	 (do-elements (con (lambda-var-constraints x) nil)
	   (when (and (eq (constraint-kind con) kind)
		      (eq (constraint-not-p con) not-p)
		      (type= (constraint-y con) y))
	     (return con))))
	(constant
	 (do-elements (con (lambda-var-constraints x) nil)
	   (when (and (eq (constraint-kind con) kind)
		      (eq (constraint-not-p con) not-p)
		      (eq (constraint-y con) y))
	     (return con))))
	(lambda-var
	 (do-elements (con (lambda-var-constraints x) nil)
	   (when (and (eq (constraint-kind con) kind)
		      (eq (constraint-not-p con) not-p)
		      (let ((cx (constraint-x con)))
			(eq (if (eq cx x)
				(constraint-y con)
				cx)
			    y)))
	     (return con)))))
      (let ((new (make-constraint (incf *constraint-number*) kind x y not-p)))
	(sset-adjoin new (lambda-var-constraints x))
	(when (lambda-var-p y)
	  (sset-adjoin new (lambda-var-constraints y)))
	new)))

;;; OK-REF-LAMBDA-VAR  --  Internal
;;;
;;; If Ref is to a Lambda-Var with Constraints (i.e. we can do flow
;;; analysis on it), then return the Lambda-Var, otherwise NIL.
;;;
(proclaim '(inline ok-ref-lambda-var))
(defun ok-ref-lambda-var (ref)
  (declare (type ref ref))
  (let ((leaf (ref-leaf ref)))
    (when (and (lambda-var-p leaf)
	       (lambda-var-constraints leaf))
      leaf)))

;;; OK-CONT-LAMBDA-VAR  --  Internal
;;;
;;; If Cont's Use is a Ref, then return OK-REF-LAMBDA-VAR of the Use,
;;; otherwise NIL.
;;;
(proclaim '(inline ok-cont-lambda-var))
(defun ok-cont-lambda-var (cont)
  (declare (type continuation cont))
  (let ((use (continuation-use cont)))
    (when (ref-p use)
      (ok-ref-lambda-var use))))

;;; ADD-TEST-CONSTRAINT  --  Internal
;;;
;;; Add the indicated test constraint to Block, marking the block as having
;;; a new assertion when the constriant was not already present.  We don't
;;; add the constraint if the block has multiple predecessors, since it
;;; only holds on this particular path.
;;;
(defun add-test-constraint (block fun x y not-p)
  (or (rest (block-pred block))
      (let ((con (find-constraint fun x y not-p))
	    (old (or (block-test-constraint block)
		     (setf (block-test-constraint block) (make-sset)))))
	(when (sset-adjoin con old)
	  (setf (block-type-asserted block) t))))
  (undefined-value))

;;; ADD-COMPLEMENT-CONSTRAINTS  --  Internal
;;;
;;; Add complementary constraints to the consequent and alternative blocks
;;; of If.  We do nothing if X is NIL.
;;;
(proclaim '(inline add-complement-constraints))
(defun add-complement-constraints (if fun x y not-p)
  (when x
    (add-test-constraint (if-consequent if) fun x y not-p)
    (add-test-constraint (if-alternative if) fun x y (not not-p)))
  (undefined-value))

;;; ADD-TEST-CONSTRAINTS  --  Internal
;;;
;;; Add test constraints to the consequent and alternative blocks of the
;;; test represented by Use.
;;;
(defun add-test-constraints (use if)
  (declare (type node use) (type cif if))
  (typecase use
    (ref
     (add-complement-constraints if 'typep (ok-ref-lambda-var use)
				 *null-type* t))
    (combination
     (let ((name (continuation-function-name
		  (basic-combination-fun use)))
	   (args (basic-combination-args use)))
       (case name
	 ((%typep %instance-typep)
	  (let ((type (second args)))
	    (when (constant-continuation-p type)
	      (let ((val (continuation-value type)))
	      (add-complement-constraints if 'typep
					  (ok-cont-lambda-var (first args))
					  (if (ctype-p val)
					      val
					      (specifier-type val))
					  nil)))))
	 ((eq eql)
	  (let* ((var1 (ok-cont-lambda-var (first args)))
		 (arg2 (second args))
		 (var2 (ok-cont-lambda-var arg2)))
	    (cond ((not var1))
		  (var2
		   (add-complement-constraints if 'eql var1 var2 nil))
		  ((constant-continuation-p arg2)
		   (add-complement-constraints if 'eql var1
					       (ref-leaf
						(continuation-use arg2))
					       nil)))))
	 ((< >)
	  (let* ((arg1 (first args))
		 (var1 (ok-cont-lambda-var arg1))
		 (arg2 (second args))
		 (var2 (ok-cont-lambda-var arg2)))
	    (when var1
	      (add-complement-constraints if name var1 (continuation-type arg2)
					  nil))
	    (when var2
	      (add-complement-constraints if (if (eq name '<) '> '<)
					  var2 (continuation-type arg1)
					  nil))))
	 (t
	  (let ((ptype (gethash name (backend-predicate-types *backend*))))
	    (when ptype
	      (add-complement-constraints if 'typep
					  (ok-cont-lambda-var (first args))
					  ptype nil))))))))
  (undefined-value))

;;; FIND-TEST-CONSTRAINTS  --  Internal
;;;
;;; Set the Test-Constraint in the successors of Block according to the
;;; condition it tests.
;;;
(defun find-test-constraints (block)
  (declare (type cblock block))
  (let ((last (block-last block)))
    (when (if-p last)
      (let ((use (continuation-use (if-test last))))
	(when use
	  (add-test-constraints use last)))))

  (setf (block-test-modified block) nil)
  (undefined-value))

;;; FIND-BLOCK-TYPE-CONSTRAINTS  --  Internal
;;;
;;; Compute the initial flow analysis sets for Block:
;;; -- For any lambda-var ref with a type check, add that constraint.
;;; -- For any lambda-var set, delete all constraints on that var, and add
;;;    those constraints to the set nuked (FIX killed?) by this block.
;;;
(defun find-block-type-constraints (block)
  (declare (type cblock block))
  (let ((gen (make-sset)))
    (collect ((kill nil adjoin))

      (let ((test (block-test-constraint block)))
	(when test
	  (sset-union gen test)))

      (do-nodes (node cont block)
	(typecase node
	  (ref
	   (when (continuation-type-check cont)
	     (let ((var (ok-ref-lambda-var node)))
	       (when var
		 (let* ((atype (continuation-type cont))
			(con (find-constraint 'typep var atype nil)))
		   (sset-adjoin con gen))))))
	  (cset
	   (let ((var (set-var node)))
	     (when (lambda-var-p var)
	       (kill var)
	       (let ((cons (lambda-var-constraints var)))
		 (when cons
		   (sset-difference gen cons))))))))

      (setf (block-in block) nil)
      (setf (block-gen block) gen)
      (setf (block-kill block) (kill))
      (setf (block-out block) (copy-sset gen))
      (setf (block-type-asserted block) nil)
      (undefined-value))))

;;; INTEGER-TYPE-P  --  Internal
;;;
;;; Return true if X is an integer NUMERIC-TYPE.
;;;
(defun integer-type-p (x)
  (declare (type ctype x))
  (and (numeric-type-p x)
       (eq (numeric-type-class x) 'integer)
       (eq (numeric-type-complexp x) :real)))

;;; CONSTRAIN-INTEGER-TYPE  --  Internal
;;;
;;; Given that an inequality holds on values of type X any Y, return a new
;;; type for X.  If Greater is true, then X was greater than Y, otherwise
;;; less.  If Or-Equal is true, then the inequality was inclusive, i.e. >=.
;;;
;;; If Greater (or not), then we max (or min) in Y's lower (or upper) bound
;;; into X and return that result.  If not Or-Equal, we can go one greater
;;; (less) than Y's bound.
;;;
(defun constrain-integer-type (x y greater or-equal)
  (declare (type numeric-type x y))
  (flet ((exclude (x)
	   (cond ((not x) nil)
		 (or-equal x)
		 (greater (1+ x))
		 (t (1- x))))
	 (bound (x)
	   (if greater (numeric-type-low x) (numeric-type-high x)))
	 (validate (x)
	   (if (and (numeric-type-low x) (numeric-type-high x)
		    (> (numeric-type-low x) (numeric-type-high x)))
	       *empty-type*
	       x)))
    (let* ((x-bound (bound x))
	   (y-bound (exclude (bound y)))
	   (new-bound (cond ((not x-bound) y-bound)
			    ((not y-bound) x-bound)
			    (greater (max x-bound y-bound))
			    (t (min x-bound y-bound))))
	   (res (copy-numeric-type x)))
      (if greater
	  (setf (numeric-type-low res) new-bound)
	  (setf (numeric-type-high res) new-bound))
      (validate res))))

;;; FLOAT-TYPE-P  --  Internal
;;;
;;; Return true if X is a float NUMERIC-TYPE.
;;;
(defun float-type-p (x)
  (declare (type ctype x))
  (and (numeric-type-p x)
       (eq (numeric-type-class x) 'float)
       (eq (numeric-type-complexp x) :real)))

;;; CONSTRAIN-FLOAT-TYPE  --  Internal
;;;
;;; Exactly the same as CONSTRAIN-INTEGER-TYPE, but for float numbers.
;;;
(defun constrain-float-type (x y greater or-equal)
  (declare (type numeric-type x y))
  (labels ((exclude (x)
	     (cond ((not x) nil)
		   (or-equal x)
		   ((consp x) x)
		   (t (list x))))
	   (bound (x)
	     (if greater (numeric-type-low x) (numeric-type-high x)))
	   (max-lower-bound (x y)
	     (cond ((< (bound-value x) (bound-value y)) y)
		   ((> (bound-value x) (bound-value y)) x)
		   ((not or-equal) (list (bound-value x)))
		   ((consp x) x)
		   (t y)))
	   (min-upper-bound (x y)
	     (cond ((< (bound-value x) (bound-value y)) x)
		   ((> (bound-value x) (bound-value y)) y)
		   ((not or-equal) (list (bound-value x)))
		   ((consp x) x)
		   (t y)))
	   (validate (x)
	     (let* ((low (numeric-type-low x))
		    (low-val (bound-value low))
		    (high (numeric-type-high x))
		    (high-val (bound-value high)))
	       (if (and low high (or (> low-val high-val)
				     (and (= low-val high-val)
					  (or (consp low) (consp high)))))
		   *empty-type*
		   x))))
    (let* ((x-bound (bound x))
	   (y-bound (exclude (bound y)))
	   (new-bound (cond ((not x-bound) y-bound)
			    ((not y-bound) x-bound)
			    (greater (max-lower-bound x-bound y-bound))
			    (t (min-upper-bound x-bound y-bound))))
	   (res (copy-numeric-type x)))
      (if greater
	  (setf (numeric-type-low res) new-bound)
	  (setf (numeric-type-high res) new-bound))
      (validate res))))

;;; CONSTRAIN-REF-TYPE  --  Internal
;;;
;;; Given the set of Constraints for a variable and the current set of
;;; restrictions from flow analysis In, set the type for Ref accordingly.
;;;
(defun constrain-ref-type (ref constraints in)
  (declare (type ref ref) (type sset constraints in))
  (let ((var-cons (copy-sset constraints)))
    (sset-intersection var-cons in)
    (let ((res (single-value-type (node-derived-type ref)))
	  (not-res *empty-type*)
	  (leaf (ref-leaf ref)))
      (do-elements (con var-cons)
	(let* ((x (constraint-x con))
	       (y (constraint-y con))
	       (not-p (constraint-not-p con))
	       (other (if (eq x leaf) y x))
	       (kind (constraint-kind con)))
	  (case kind
	    (typep
	     (if not-p
		 (setq not-res (type-union not-res other))
		 (setq res (type-intersection res other))))
	    (eql
	     (let ((other-type (leaf-type other)))
	       (if not-p
		   (when (and (constant-p other)
			      (member-type-p other-type))
		     (setq not-res (type-union not-res other-type)))
		   (let ((leaf-type (leaf-type leaf)))
		     (when (or (constant-p other)
			       (and (csubtypep other-type leaf-type)
				    (not (type= other-type leaf-type))))
		       (change-ref-leaf ref other)
		       (when (constant-p other) (return)))))))
	    ((< >)
	     (cond ((and (integer-type-p res) (integer-type-p y))
		    (let ((greater (eq kind '>)))
		      (let ((greater (if not-p (not greater) greater)))
			(setq res
			      (constrain-integer-type res y greater not-p)))))
		   #+constrain-float-type
		   ((and (float-type-p res) (float-type-p y))
		    (let ((greater (eq kind '>)))
		      (let ((greater (if not-p (not greater) greater)))
			(setq res
			      (constrain-float-type res y greater not-p)))))
		   )))))

      (let* ((cont (node-cont ref))
	     (dest (continuation-dest cont)))
	(cond ((and (if-p dest)
		    (csubtypep *null-type* not-res)
		    (eq (continuation-asserted-type cont) *wild-type*))
	       (setf (node-derived-type ref) *wild-type*)
	       (change-ref-leaf ref (find-constant 't)))
	      (t
	       (derive-node-type ref (or (type-difference res not-res)
					 res)))))))

  (undefined-value))

;;; USE-RESULT-CONSTRAINTS  --  Internal
;;;
;;; Deliver the results of constraint propagation to REFs in Block.  During
;;; this pass, we also do local constraint propagation by adding in
;;; constraints as we see them during the pass through the block.
;;;
(defun use-result-constraints (block)
  (declare (type cblock block))
  (let ((in (block-in block)))

    (let ((test (block-test-constraint block)))
      (when test
	(sset-union in test)))

    (do-nodes (node cont block)
      (typecase node
	(ref
	 (let ((var (ref-leaf node)))
	   (when (lambda-var-p var)
	     (let ((con (lambda-var-constraints var)))
	       (when con
		 (constrain-ref-type node con in)
		 (when (continuation-type-check cont)
		   (sset-adjoin
		    (find-constraint 'typep var
				     (single-value-type
				      (continuation-asserted-type cont))
				     nil)
		    in)))))))
	(cset
	 (let ((var (set-var node)))
	   (when (lambda-var-p var)
	     (let ((cons (lambda-var-constraints var)))
	       (when cons
		 (sset-difference in cons))))))))))

;;; CLOSURE-VAR-P  --  Internal
;;;
;;; Return true if Var would have to be closed over if environment analysis
;;; ran now (i.e. if there are any uses that have a different home lambda
;;; than the var's home.)
;;;
(defun closure-var-p (var)
  (declare (type lambda-var var))
  (let ((home (lambda-home (lambda-var-home var))))
    (flet ((frob (l)
	     (dolist (node l nil)
	       (fi (eq (node-home-lambda node) home)
		   (return t)))))
      (or (frob (leaf-refs var))
	  (frob (basic-var-sets var))))))

;;; INIT-VAR-CONSTRAINTS  --  Internal
;;;
;;; Give an empty constraints set to any var that doesn't have one and
;;; isn't a set closure var.  Since a var that we previously rejected looks
;;; identical to one that is new, FIX? so we optimistically keep hoping that
;;; vars stop being closed over or lose their sets.
;;;
(defun init-var-constraints (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    ; FIX mv flet before dolist?
    (flet ((frob (x)
	     (dolist (var (lambda-vars x))
	       (fi (lambda-var-constraints var)  ; FIX or
		   (when (or (null (lambda-var-sets var))
			     (not (closure-var-p var)))
		     (setf (lambda-var-constraints var) (make-sset)))))))
      (frob fun)
      (dolist (let (lambda-lets fun))
	(frob let)))))

;;; FLOW-PROPAGATE-CONSTRAINTS  --  Internal
;;;
;;; BLOCK-IN becomes the intersection of the OUT of the prececessors.  Our
;;; OUT is:
;;;     out U (in - kill)
;;;
;;; BLOCK-KILL is just a list of the lambda-vars killed, so we must compute
;;; the kill set when there are any vars killed.  We bum this a bit by
;;; special-casing when only one var is killed, and just using that var's
;;; constraints as the kill set.  This set could possibly be precomputed,
;;; but it would have to be invalidated whenever any constraint is added,
;;; which would be a pain.
;;;
(defun flow-propagate-constraints (block)
  (let* ((pred (block-pred block))
	 (in (cond (pred
		    (let ((res (copy-sset (block-out (first pred)))))
		      (dolist (b (rest pred))
			(sset-intersection res (block-out b)))
		      res))
		   (t
		    (when *check-consistency*
		      (let ((*compiler-error-context* (block-last block)))
			(compiler-warning
			 "*** Unreachable code in constraint ~
			  propagation...  Bug?")))
		    (make-sset))))
	 (kill (block-kill block))
	 (out (block-out block)))

    (setf (block-in block) in)
    (cond ((null kill)
	   (sset-union (block-out block) #| FIX just out? |# in))
	  ((null (rest kill))
	   (let ((con (lambda-var-constraints (first kill))))
	     (if con
		 (sset-union-of-difference out in con)
		 (sset-union out in))))
	  (t
	   (let ((kill-set (make-sset)))
	     (dolist (var kill)
	       (let ((con (lambda-var-constraints var)))
		 (when con
		   (sset-union kill-set con))))
	     (sset-union-of-difference (block-out block) in kill-set))))))

;;; CONSTRAINT-PROPAGATE  --  Interface
;;;
;;; FIX describe
;;;
(defun constraint-propagate (component)
  (declare (type component component))
  (init-var-constraints component)

  (do-blocks (block component)
    (when (block-test-modified block)
      (find-test-constraints block)))

  (do-blocks (block component)
    (cond ((block-type-asserted block)
	   (find-block-type-constraints block))
	  (t
	   (setf (block-in block) nil)
	   (setf (block-out block) (copy-sset (block-gen block))))))

  (setf (block-out (component-head component)) (make-sset))

  (let ((did-something nil))
    (loop
      (do-blocks (block component)
	(when (flow-propagate-constraints block)
	  (setq did-something t)))

      (or did-something (return))
      (setq did-something nil)))

  (do-blocks (block component)
    (use-result-constraints block))

  (undefined-value))
