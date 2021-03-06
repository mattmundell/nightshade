;;; Structures for the second intermediate representation in the compiler,
;;; IR2 (also known as the virtual machine representation, VMR).

(in-package "C")

(export '(tn-ref tn-ref-p make-tn-ref tn-ref-tn tn-ref-write-p
	  tn-ref-next tn-ref-vop tn-ref-next-ref tn-ref-across
	  tn-ref-target tn-ref-load-tn
	  ;;
	  sb sb-p sb-name sc sc-p sc-name sc-number sc-sb
	  ;;
	  tn tn-p make-random-tn tn-sc tn-offset
	  ;;
	  ;; FIX Call.lisp and core.lisp need to get at these slots.
	  ir2-component-constants ir2-environment-number-stack-p
	  ;; New-assem.lisp needs these.
	  ir2-component-dyncount-info ir2-block-block vop-block))


;;; FIX should be where?
#[ VOP Definition

Before the operand TN-refs are passed to the emit function, the following
stuff is done:
 -- The refs in the operand and result lists are linked together in order using
    the Across slot.  This list is properly NIL terminated.
 -- The TN slot in each ref is set, and the ref is linked into that TN's refs
    using the Next slot.
 -- The Write-P slot is set depending on whether the ref is an argument or
    result.
 -- The other slots have the default values.

The template emit function fills in the Vop, Costs, Cost-Function,
SC-Restriction and Preference slots, and links together the Next-Ref chain as
appropriate.

== Lifetime model ==

    Note in doc that the same TN may not be used as both a more operand and as any
    other operand to the same VOP, to simplify more operand LTN number coalescing.

It seems we need a fairly elaborate model for intra-VOP conflicts in order to
allocate temporaries without introducing spurious conflicts.  Consider the
important case of a VOP such as a miscop that must have operands in certain
registers.  We allocate a wired temporary, create a local preference for the
corresponding operand, and move to (or from) the temporary.  If all temporaries
conflict with all arguments, the result will be correct, but arguments could
never be packed in the actual passing register.  If temporaries didn't conflict
with any arguments, then the temporary for an earlier argument might get packed
in the same location as the operand for a later argument; loading would then
destroy an argument before it was read.

A temporary's intra-VOP lifetime is represented by the times at which its life
starts and ends.  There are various instants during the evaluation that start
and end VOP lifetimes.  Two TNs conflict if the live intervals overlap.
Lifetimes are open intervals: if one TN's lifetime begins at a point where
another's ends, then the TNs don't conflict.

The times within a VOP are the following:

:Load
    This is the beginning of the argument's lives, as far as intra-vop
    conflicts are concerned.  If load-TNs are allocated, then this is the
    beginning of their lives.

(:Argument <n>)
    The point at which the N'th argument is read for the last time (by this
    VOP).  If the argument is dead after this VOP, then the argument becomes
    dead at this time, and may be reused as a temporary or result load-TN.

(:Eval <n>)
    The N'th evaluation step.  There may be any number of evaluation steps, but
    it is unlikely that more than two are needed.

(:Result <n>)
    The point at which the N'th result is first written into.  This is the
    point at which that result becomes live.

:Save
    Similar to :Load, but marks the end of time.  This is point at which result
    load-TNs are stored back to the actual location.

In any of the list-style time specifications, the keyword by itself stands for
the first such time, i.e.
    :argument  <=>  (:argument 0)


Note that argument/result read/write times don't actually have to be in the
order specified, but they must *appear* to happen in that order as far as
conflict analysis is concerned.  For example, the arguments can be read in any
order as long no TN is written that has a life beginning at or after
(:Argument <n>), where N is the number of an argument whose reading was
postponed.

    XXX (???)

    We probably also want some syntactic sugar in Define-VOP for automatically
    moving operands to/from explicitly allocated temporaries so that this kind of
    thing is somewhat easy.  There isn't really any reason to consider the
    temporary to be a load-TN, but we want to compute costs as though it was and
    want to use the same operand loading routines.

    We also might consider allowing the lifetime of an argument/result to be
    extended forward/backward.  This would in many cases eliminate the need for
    temporaries when operands are read/written out of order.


== VOP Cost model ==

Note that in this model, if a operand has no restrictions, it has no cost.
This makes make sense, since the purpose of the cost is to indicate the
relative value of packing in different SCs.  If the operand isn't required to
be in a good SC (i.e. a register), then we might as well leave it in memory.
The SC restriction mechanism can be used even when doing a move into the SC is
too complex to be generated automatically (perhaps requiring temporary
registers), since Define-VOP allows operand loading to be done explicitly.

== Efficiency notes ==

  In addition to
being used to tell whether a particular unsafe template might get emitted, we
can also use it to give better efficiency notes:
 -- We can say what is wrong with the call types, rather than just saying we
    failed to open-code.
 -- We can tell whether any of the "better" templates could possibly apply,
    i.e. is the inapplicability of a template because of inadequate type
    information or because the type is just plain wrong.  We don't want to
    flame people when a template that couldn't possibly match doesn't match,
    e.g. complaining that we can't use fixnum+ when the arguments are known to
    be floats.


This is how we give better efficiency notes:

The Template-Note is a short noun-like string without capitalization or
punctuation that describes what the template "does", i.e. we say
"Unable to do ~A, doing ~A instead."

The Cost is moved from the Vop-Info to the Template structure, and is used to
determine the "goodness" of possibly applicable templates.  [Could flush
Template/Vop-Info distinction]  The cost is used to choose the best applicable
template to emit, and also to determine what better templates we might have
been able to use.

A template is possibly applicable if there is an intersection between all of
the arg/result types and the corresponding arg/result restrictions, i.e. the
template is not clearly impossible: more declarations might allow it to be
emitted.
]#

(eval-when (compile load eval)

;;;
;;; The largest number of TNs whose liveness changes that we can have in any
;;; block.
(defconstant local-tn-limit 64)

(deftype local-tn-number () `(integer 0 (,local-tn-limit)))
(deftype local-tn-count () `(integer 0 ,local-tn-limit))
(deftype local-tn-vector () `(simple-vector ,local-tn-limit))
(deftype local-tn-bit-vector () `(simple-bit-vector ,local-tn-limit))

;;; Type of an SC number.
(deftype sc-number () `(integer 0 (,sc-number-limit)))

;;; Types for vectors indexed by SC numbers.
(deftype sc-vector () `(simple-vector ,sc-number-limit))
(deftype sc-bit-vector () `(simple-bit-vector ,sc-number-limit))

;;; The different policies we can use to determine the coding strategy.
;;;
(deftype policies ()
  '(member :safe :small :fast :fast-safe))

); Eval-When (Compile Load Eval)


;;;; Primitive types.
;;;
;;; The primitive type is used to represent the aspects of type interesting
;;; to the VM.  Selection of IR2 translation templates is done on the basis
;;; of the primitive types of the operands, and the primitive type of a
;;; value is used to constrain the possible representations of that value.
;;;
;;; FIX should be where?
#[ Standard Primitives
[FIX]
]#
;;;
;;;
(eval-when (compile load eval)
;;;
(defstruct (primitive-type (:print-function %print-primitive-type))
  ;;
  ;; The name of this primitive-type.
  (name nil :type symbol)
  ;;
  ;; A list of the SC numbers for all the SCs that a TN of this type can be
  ;; allocated in.
  (scs nil :type list)
  ;;
  ;; The Lisp type equivalent to this type.  If this type could never be
  ;; returned by Primitive-Type, then this is the NIL (or empty) type.
  (type (required-argument) :type ctype)
  ;;
  ;; The template used to check that an object is of this type.  This is
  ;; a template of one argument and one result, both of primitive-type T.  If
  ;; the argument is of the correct type, then it is delivered into the result.
  ;; If the type is incorrect, then an error is signalled.
  (check nil :type (or template null)))

(defprinter primitive-type
  name
  (type :test (and type
		   (not (eq (type-specifier type)
			    (primitive-type-name structure))))
	:prin1 (type-specifier type)))

); eval-when (compile load eval)


;;;; IR1 annotations used for IR2 conversion.
;;;
;;; Block-Info
;;;    Holds the IR2-Block structure.  If there are overflow blocks, then this
;;;    points to the first IR2-Block.  The Block-Info of the dummy component
;;;    head and tail are dummy IR2 blocks that begin and end the emission order
;;;    thread.
;;;
;;; Component-Info
;;;    Holds the IR2-Component structure.
;;;
;;; Continuation-Info
;;;    Holds the IR2-Continuation structure.  Continuations whose values aren't
;;;    used won't have any.
;;;
;;; Cleanup-Info
;;;    If non-null, then a TN in which the affected dynamic environment pointer
;;;    should be saved after the binding is instantiated.
;;;
;;; Environment-Info
;;;    Holds the IR2-Environment structure.
;;;
;;; Tail-Set-Info
;;;    Holds the Return-Info structure.
;;;
;;; NLX-Info-Info
;;;    Holds the IR2-NLX-Info structure.
;;;
;;; Leaf-Info
;;;    If a non-set lexical variable, the TN that holds the value in the home
;;;    environment.  If a constant, then the corresponding constant TN.
;;;    If an XEP lambda, then the corresponding Entry-Info structure.
;;;
;;; Basic-Combination-Info
;;;    The template chosen by LTN, or
;;;        :FULL if this is definitely a full call.
;;;        :FUNNY if this is a random thing with IR2-convert.
;;;        :LOCAL if this is a local call.
;;;
;;; Node-Tail-P
;;;    After LTN analysis, this is true only in combination nodes that are
;;;    truly tail recursive.
;;;

;;; The IR2-Block structure holds information about a block that is used during
;;; and after IR2 conversion.  It is stored in the Block-Info slot for the
;;; associated block.
;;;
(defstruct (ir2-block
	    (:include block-annotation)
	    (:constructor really-make-ir2-block (block))
	    (:print-function %print-ir2-block))
  ;;
  ;; The IR2-Block's number, which differs from Block's Block-Number if any
  ;; blocks are split.  This is assigned by lifetime analysis.
  (number nil :type (or index null))
  ;;
  ;; Information about unknown-values continuations that is used by stack
  ;; analysis to do stack simulation.  An unknown-values continuation is
  ;; Pushed if it's Dest is in another block.  Similarly, a continuation is
  ;; Popped if its Dest is in this block but has its uses elsewhere.  The
  ;; continuations are in the order that are pushed/popped in the block.
  ;; Note that the args to a single MV-Combination appear reversed in
  ;; Popped, since we must effectively pop the last argument first.  All
  ;; pops must come before all pushes (although internal MV uses may be
  ;; interleaved.)  Popped is computed by LTN, and Pushed is computed by
  ;; stack analysis.
  (pushed () :type list)
  (popped () :type list)
  ;;
  ;; The result of stack analysis: lists of all the unknown-values
  ;; continuations on the stack at the block start and end, topmost
  ;; continuation first.
  (start-stack () :type list)
  (end-stack () :type list)
  ;;
  ;; The first and last VOP in this block.  If there are none, both slots are
  ;; null.
  (start-vop nil :type (or vop null))
  (last-vop nil :type (or vop null))
  ;;
  ;; Number of local TNs actually allocated.
  (local-tn-count 0 :type local-tn-count)
  ;;
  ;; A vector that maps local TN numbers to TNs.  Some entries may be NIL,
  ;; indicating that that number is unused.  (This allows us to delete local
  ;; conflict information without compressing the LTN numbers.)
  ;;
  ;; If an entry is :More, then this block contains only a single VOP.  This
  ;; VOP has so many more arguments and/or results that they cannot all be
  ;; assigned distinct LTN numbers.  In this case, we assign all the more args
  ;; one LTN number, and all the more results another LTN number.  We can do
  ;; this, since more operands are referenced simultaneously as far as conflict
  ;; analysis is concerned.  Note that all these :More TNs will be global TNs.
  (local-tns (make-array local-tn-limit) :type local-tn-vector)
  ;;
  ;; Bit-vectors used during lifetime analysis to keep track of references to
  ;; local TNs.  When indexed by the LTN number, the index for a TN is non-zero
  ;; in Written if it is ever written in the block, and in Live-Out if
  ;; the first reference is a read.
  (written (make-array local-tn-limit :element-type 'bit
		       :initial-element 0)
	   :type local-tn-bit-vector)
  (live-out (make-array local-tn-limit :element-type 'bit)
	    :type local-tn-bit-vector)
  ;;
  ;; Similar to the above, but is updated by lifetime flow analysis to have a 1
  ;; for LTN numbers of TNs live at the end of the block.  This takes into
  ;; account all TNs that aren't :Live.
  (live-in (make-array local-tn-limit :element-type 'bit
		       :initial-element 0)
	   :type local-tn-bit-vector)
  ;;
  ;; A thread running through the global-conflicts structures for this block,
  ;; sorted by TN number.
  (global-tns nil :type (or global-conflicts null))
  ;;
  ;; The assembler label that points to the beginning of the code for this
  ;; block.  Null when we haven't assigned a label yet.
  (%label nil)
  ;;
  ;; List of Location-Info structures describing all the interesting (to the
  ;; debugger) locations in this block.
  (locations nil :type list))

(defprinter ir2-block
  (pushed :test pushed)
  (popped :test popped)
  (start-vop :test start-vop)
  (last-vop :test last-vop)
  (local-tn-count :test (not (zerop local-tn-count)))
  (%label :test %label))

;;; The IR2-Continuation structure is used to annotate continuations that are
;;; used as a function result continuation or that receive MVs.
;;;
(defstruct (ir2-continuation
	    (:constructor make-ir2-continuation (primitive-type))
	    (:print-function %print-ir2-continuation))
  ;;
  ;; If this is :Delayed, then this is a single value continuation for which
  ;; the evaluation of the use is to be postponed until the evaluation of
  ;; destination.  This can be done for ref nodes or predicates whose
  ;; destination is an IF.
  ;;
  ;; If this is :Fixed, then this continuation has a fixed number of values,
  ;; with the TNs in Locs.
  ;;
  ;; If this is :Unknown, then this is an unknown-values continuation, using
  ;; the passing locations in Locs.
  ;;
  ;; If this is :Unused, then this continuation should never actually be used
  ;; as the destination of a value: it is only used tail-recursively.
  (kind :fixed :type (member :delayed :fixed :unknown :unused))
  ;;
  ;; The primitive-type of the first value of this continuation.  This is
  ;; primarily for internal use during LTN, but it also records the type
  ;; restriction on delayed references.  In multiple-value contexts, this is
  ;; null to indicate that it is meaningless.  This is always (primitive-type
  ;; (continuation-type cont)), which may be more restrictive than the
  ;; tn-primitive-type of the value TN.  This is becase the value TN must hold
  ;; any possible type that could be computed (before type checking.)
  (primitive-type nil :type (or primitive-type null))
  ;;
  ;; Locations used to hold the values of the continuation.  If the number
  ;; of values if fixed, then there is one TN per value.  If the number of
  ;; values is unknown, then this is a two-list of TNs holding the start of the
  ;; values glob (FIX ?) and the number of values.  Note that since type checking is
  ;; the responsibility of the values receiver, these TNs primitive type is
  ;; only based on the proven type information.
  (locs nil :type list))

(defprinter ir2-continuation
  kind
  primitive-type
  locs)

;;; The IR2-Component serves mostly to accumulate non-code information about
;;; the component being compiled.
;;;;
(defstruct ir2-component
  ;;
  ;; The counter used to allocate global TN numbers.
  (global-tn-counter 0 :type index)
  ;;
  ;; Normal-TNs is the head of the list of all the normal TNs that need to be
  ;; packed, linked through the Next slot.  We place TNs on this list when we
  ;; allocate them so that Pack can find them.
  ;;
  ;; Restricted-TNs are TNs that must be packed within a finite SC.  We pack
  ;; these TNs first to ensure that the restrictions will be satisfied (if
  ;; possible).
  ;;
  ;; Wired-TNs are TNs that must be packed at a specific location.  The SC
  ;; and Offset are already filled in.
  ;;
  ;; Constant-TNs are non-packed TNs that represent constants.  :Constant TNs
  ;; may eventually be converted to :Cached-Constant normal TNs.
  (normal-tns nil :type (or tn null))
  (restricted-tns nil :type (or tn null))
  (wired-tns nil :type (or tn null))
  (constant-tns nil :type (or tn null))
  ;;
  ;; A list of all the :COMPONENT TNs (live throughout the component.)  These
  ;; TNs will also appear in the {NORMAL,RESTRICTED,WIRED} TNs as appropriate
  ;; to their location.
  (component-tns () :type list)
  ;;
  ;; If this component has a NFP, then this is it.
  (nfp nil :type (or tn null))
  ;;
  ;; A list of the explicitly specified save TNs (kind :SPECIFIED-SAVE).  These
  ;; TNs will also appear in the {NORMAL,RESTRICTED,WIRED} TNs as appropriate
  ;; to their location.
  (specified-save-tns () :type list)
  ;;
  ;; A list of all the blocks whose ir2-block has a non-null value for
  ;; Popped.  This slot is initialized by `ltn-analyze' as an input to
  ;; `stack-analyze'.
  (values-receivers nil :type list)
  ;;
  ;; An adjustable vector that records all the constants in the constant pool.
  ;; A non-immediate :Constant TN with offset 0 refers to the constant in
  ;; element 0, etc.  Normal constants are represented by the placing the
  ;; Constant leaf in this vector.  A load-time constant is distinguished by
  ;; being a cons (Kind . What).  Kind is a keyword indicating how the constant
  ;; is computed, and What is some context.
  ;;
  ;; These load-time constants are recognized:
  ;;
  ;; (:entry . <function>)
  ;;    Is replaced by the code pointer for the specified function.  This is
  ;; 	how compiled code (including DEFUN) gets its hands on a function.
  ;; 	<function> is the XEP lambda for the called function; it's Leaf-Info
  ;; 	should be an Entry-Info structure.
  ;;
  ;; (:label . <label>)
  ;;    Is replaced with the byte offset of that label from the start of the
  ;;    code vector (including the header length.)
  ;;
  ;; A null entry in this vector is a placeholder for implementation overhead
  ;; that is eventually stuffed in somehow.
  ;;
  (constants (make-array 10 :fill-pointer 0 :adjustable t) :type vector)
  ;;
  ;; Some kind of info about the component's run-time representation.  This is
  ;; filled in by the VM supplied Select-Component-Format function.
  format
  ;;
  ;; A list of the Entry-Info structures describing all of the entries into
  ;; this component.  Filled in by entry analysis.
  (entries nil :type list)
  ;;
  ;; Head of the list of :ALIAS TNs in this component, threaded by TN-NEXT.
  (alias-tns nil :type (or tn null))
  ;;
  ;; Spilled-VOPs is a hashtable translating from "interesting" VOPs to a list
  ;; of the TNs spilled at that VOP.  This is used when computing debug info so
  ;; that we don't consider the TN's value to be valid when it is in fact
  ;; somewhere else.  Spilled-TNs has T for every "interesting" TN that is ever
  ;; spilled, providing a representation that is more convenient some places.
  (spilled-vops (make-hash-table :test #'eq) :type hash-table)
  (spilled-tns (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; Dynamic vop count info.  This is needed by both ir2-convert and
  ;; setup-dynamic-count-info.  (But only if we are generating code to
  ;; collect dynamic statistics.)
  (dyncount-info nil :type (or null dyncount-info)))

;;; The Entry-Info structure condenses all the information that the dumper
;;; needs to create each XEP's function entry data structure.  The Entry-Info
;;; structures are somtimes created before they are initialized, since ir2
;;; conversion may need to compile a forward reference.  In this case
;;; the slots aren't actually initialized until entry analysis runs.
;;;
(defstruct entry-info
  ;;
  ;; True if this function has a non-null closure environment.
  (closure-p nil :type boolean)
  ;;
  ;; A label pointing to the entry vector for this function.  Null until
  ;; ENTRY-ANALYZE runs.
  (offset nil :type (or label null))
  ;;
  ;; If this function was defined using DEFUN, then this is the name of the
  ;; function, a symbol or (SETF <symbol>).  Otherwise, this is some string
  ;; that is intended to be informative.
  (name "<not computed>" :type (or simple-string list symbol))
  ;;
  ;; A string representing the argument list that the function was defined
  ;; with.
  (arguments nil :type (or simple-string null))
  ;;
  ;; A function type specifier representing the arguments and results of this
  ;; function.
  (type 'function :type (or list (member function))))

;;; The IR2-Environment is used to annotate non-let lambdas with their passing
;;; locations.  It is stored in the Environment-Info.
;;;
(defstruct (ir2-environment
	    (:print-function %print-ir2-environment))
  ;;
  ;; The TNs that hold the passed environment within the function.  This is an
  ;; alist translating from the NLX-Info or lambda-var to the TN that holds
  ;; the corresponding value within this function.  This list is in the same
  ;; order as the ENVIRONMENT-CLOSURE.
  (environment nil :type list)
  ;;
  ;; The TNs that hold the Old-Fp and Return-PC within the function.  We
  ;; always save these so that the debugger can do a backtrace, even if the
  ;; function has no return (and thus never uses them).  Null only temporarily.
  (old-fp nil :type (or tn null))
  (return-pc nil :type (or tn null))
  ;;
  ;; The passing location for the Return-PC.  The return PC is treated
  ;; differently from the other arguments, since in some implementations we may
  ;; use a call instruction that requires the return PC to be passed in a
  ;; particular place.
  (return-pc-pass (required-argument) :type tn)
  ;;
  ;; True if this function has a frame on the number stack.  This is set by
  ;; representation selection whenever it is possible that some function in
  ;; our tail set will make use of the number stack.
  (number-stack-p nil :type boolean)
  ;;
  ;; A list of all the :Environment TNs live in this environment.
  (live-tns nil :type list)
  ;;
  ;; A list of all the :Debug-Environment TNs live in this environment.
  (debug-live-tns nil :type list)
  ;;
  ;; A label that marks the start of elsewhere code for this function.  Null
  ;; until this label is assigned by codegen.  Used for maintaining the debug
  ;; source map.
  (elsewhere-start nil :type (or label null))
  ;;
  ;; A label that marks the first location in this function at which the
  ;; environment is properly initialized, i.e. arguments moved from their
  ;; passing locations, etc.  This is the start of the function as far as the
  ;; debugger is concerned.
  (environment-start nil :type (or label null)))

(defprinter ir2-environment
  environment
  old-fp
  return-pc
  return-pc-pass)

;;; The Return-Info structure is used by GTN to represent the return strategy
;;; and locations for all the functions in a given Tail-Set.  It is stored in
;;; the Tail-Set-Info.
;;;
(defstruct (return-info
	    (:print-function %print-return-info))
  ;;
  ;; The return convention used:
  ;; -- If :Unknown, we use the standard return convention.
  ;; -- If :Fixed, we use the known-values convention.
  (kind (required-argument) :type (member :fixed :unknown))
  ;;
  ;; The number of values returned, or :Unknown if we don't know.  Count may be
  ;; known when Kind is :Unknown, since we may choose the standard return
  ;; convention for other reasons.
  (count (required-argument) :type (or index (member :unknown)))
  ;;
  ;; If count isn't :Unknown, then this is a list of the primitive-types of
  ;; each value.
  (types () :type list)
  ;;
  ;; If kind is :Fixed, then this is the list of the TNs that we return the
  ;; values in.
  (locations () :type list))

(defprinter return-info
  kind
  count
  types
  locations)

(defstruct (ir2-nlx-info (:print-function %print-ir2-nlx-info))
  ;;
  ;; If the kind is :Entry (a lexical exit), then in the home environment, this
  ;; holds a Value-Cell object containing the unwind block pointer.  In the
  ;; other cases nobody directly references the unwind-block, so we leave this
  ;; slot null.
  (home nil :type (or tn null))
  ;;
  ;; The saved control stack pointer.
  (save-sp (required-argument) :type tn)
  ;;
  ;; The list of dynamic state save TNs.
  (dynamic-state (list* (make-stack-pointer-tn)
			(make-dynamic-state-tns))
		 :type list)
  ;;
  ;; The target label for NLX entry.
  (target (gen-label) :type label))

(defprinter ir2-nlx-info
  home
  save-sp
  dynamic-state)

;; FIX
#|
;;; The Loop structure holds information about a loop.
;;;
(defstruct (cloop (:print-function %print-loop)
		  (:conc-name loop-)
		  (:predicate loop-p)
		  (:constructor make-loop)
		  (:copier copy-loop))
  ;;
  ;; The kind of loop that this is.  These values are legal:
  ;;
  ;;    :Outer
  ;;        This is the outermost loop structure, and represents all the
  ;;        code in a component.
  ;;
  ;;    :Natural
  ;;        A normal loop with only one entry.
  ;;
  ;;    :Strange
  ;;        A segment of a "strange loop" in a non-reducible flow graph.
  ;;
  (kind (required-argument) :type (member :outer :natural :strange))
  ;;
  ;; The first and last blocks in the loop.  There may be more than one tail,
  ;; since there may be multiple back branches to the same head.
  (head nil :type (or cblock null))
  (tail nil :type list)
  ;;
  ;; A list of all the blocks in this loop or its inferiors that have a
  ;; successor outside of the loop.
  (exits nil :type list)
  ;;
  ;; The loop that this loop is nested within.  This is null in the outermost
  ;; loop structure.
  (superior nil :type (or cloop null))
  ;;
  ;; A list of the loops nested directly within this one.
  (inferiors nil :type list)
  ;;
  ;; The head of the list of blocks directly within this loop.  We must recurse
  ;; on Inferiors to find all the blocks.
  (blocks nil :type (or null cblock)))

(defprinter loop
  kind
  head
  tail
  exits)
|#


;;;; VOPs and Templates.

;;; A VOP is a Virtual Operation.  It represents an operation and the
;;; operands to the operation.
;;;
(defstruct (vop (:print-function %print-vop)
		(:constructor really-make-vop (block node info args results)))
  ;;
  ;; VOP-Info structure containing static info about the operation.
  (info nil :type (or vop-info null))
  ;;
  ;; The IR2-Block this VOP is in.
  (block (required-argument) :type ir2-block)
  ;;
  ;; VOPs evaluated after and before this one.  Null at the beginning/end of
  ;; the block, and temporarily during IR2 translation.
  (next nil :type (or vop null))
  (prev nil :type (or vop null))
  ;;
  ;; Heads of the TN-Ref lists for operand TNs, linked using the Across slot.
  (args nil :type (or tn-ref null))
  (results nil :type (or tn-ref null))
  ;;
  ;; Head of the list of write refs for each explicitly allocated temporary,
  ;; linked together using the Across slot.
  (temps nil :type (or tn-ref null))
  ;;
  ;; Head of the list of all TN-refs for references in this VOP, linked by the
  ;; Next-Ref slot.  There will be one entry for each operand and two (a read
  ;; and a write) for each temporary.
  (refs nil :type (or tn-ref null))
  ;;
  ;; Stuff that is passed uninterpreted from IR2 conversion to codegen.  The
  ;; meaning of this slot is totally dependent on the VOP.
  codegen-info
  ;;
  ;; Node that generated this VOP, for keeping track of debug info.
  (node nil :type (or node null))
  ;;
  ;; Local-TN bit vector representing the set of TNs live after args are read
  ;; and before results are written.  This is only filled in when
  ;; VOP-INFO-SAVE-P is non-null.
  (save-set nil :type (or local-tn-bit-vector null)))

(defprinter vop
  (info :prin1 (vop-info-name info))
  args
  results
  (codegen-info :test codegen-info))

;;; The TN-Ref structure contains information about a particular reference
;;; to a TN.  The information in the TN-Refs largely determines how TNs are
;;; packed.
;;;
(defstruct (tn-ref (:print-function %print-tn-ref)
		   (:constructor really-make-tn-ref (tn write-p)))
  ;;
  ;; The TN referenced.
  (tn (required-argument) :type tn)
  ;;
  ;; True if this is a write reference, false if a read.
  (write-p nil :type boolean)
  ;;
  ;; Thread running through all TN-Refs for this TN of the same kind (read or
  ;; write).
  (next nil :type (or tn-ref null))
  ;;
  ;; The VOP where the reference happens.  The this is null only temporarily.
  (vop nil :type (or vop null))
  ;;
  ;; Thread running through all TN-Refs in VOP, in reverse order of reference.
  (next-ref nil :type (or tn-ref null))
  ;;
  ;; Thread the TN-Refs in VOP of the same kind (argument, result, temp).
  (across nil :type (or tn-ref null))
  ;;
  ;; If true, this is a TN-Ref also in VOP whose TN we would like packed in the
  ;; same location as our TN.  Read and write refs are always paired: Target in
  ;; the read points to the write, and vice-versa.
  (target nil :type (or null tn-ref))
  ;;
  ;; Load TN allocated for this operand, if any.
  (load-tn nil :type (or tn null)))

(defprinter tn-ref
  tn
  write-p
  (vop :test vop :prin1 (vop-info-name (vop-info vop))))

;;; The Template represents a particular IR2 coding strategy for a known
;;; function.
;;;
(defstruct (template
	    (:print-function %print-template)
	    (:pure t))
  ;;
  ;; The symbol name of this VOP.  This is used when printing the VOP and is
  ;; also used to provide a handle for definition and translation.
  (name nil :type symbol)
  ;;
  ;; A Function-Type describing the arg/result type restrictions.  We compute
  ;; this from the Primitive-Type restrictions to make life easier for IR1
  ;; phases that need to anticipate LTN's template selection.
  (type (required-argument) :type function-type)
  ;;
  ;; Lists of restrictions on the argument and result types.  A restriction may
  ;; take several forms:
  ;; -- The restriction * is no restriction at all.
  ;; -- A restriction (:OR <primitive-type>*) means that the operand must have
  ;;    one of the specified primitive types.
  ;; -- A restriction (:CONSTANT <predicate> <type-spec>) means that the
  ;;    argument (not a result) must be a compile-time constant that satisfies
  ;;    the specified predicate function.  In this case, the constant value
  ;;    will be passed as an info argument rather than as a normal argument.
  ;;    <type-spec> is a Lisp type specifier for the type tested by the
  ;;    predicate, used when we want to represent the type constraint as a Lisp
  ;;    function type.
  ;;
  ;; If Result-Types is :Conditional, then this is an IF-xxx style conditional
  ;; that yeilds its result as a control transfer.  The emit function takes two
  ;; info arguments: the target label and a boolean flag indicating whether to
  ;; negate the sense of the test.
  (arg-types nil :type list)
  (result-types nil :type (or list (member :conditional)))
  ;;
  ;; The primitive type restriction applied to each extra argument or result
  ;; following the fixed operands.  If NIL, no extra args/results are allowed.
  ;; Otherwise, either * or a (:OR ...) list as described for the
  ;; {ARG,RESULT}-TYPES.
  (more-args-type nil :type (or (member nil *) cons))
  (more-results-type nil :type (or (member nil *) cons))
  ;;
  ;; If true, this is a function that is called with no arguments to see if
  ;; this template can be emitted.  This is used to conditionally compile for
  ;; different target hardware configuarations (e.g. FP hardware.)
  (guard nil :type (or function null))
  ;;
  ;; The policy under which this template is the best translation.  Note that
  ;; LTN might use this template under other policies if it can't figure our
  ;; anything better to do.
  (policy (required-argument) :type policies)
  ;;
  ;; The base cost for this template, given optimistic assumptions such as no
  ;; operand loading, etc.
  (cost (required-argument) :type index)
  ;;
  ;; If true, then a short noun-like phrase describing what this VOP "does",
  ;; i.e. the implementation strategy.  This is for use in efficiency notes.
  (note nil :type (or string null))
  ;;
  ;; The number of trailing arguments to VOP or %Primitive that we bundle into
  ;; a list and pass into the emit function.  This provides a way to pass
  ;; uninterpreted stuff directly to the code generator.
  (info-arg-count 0 :type index)
  ;;
  ;; A function that emits the VOPs for this template.  Arguments:
  ;;  1] Node for source context.
  ;;  2] IR2-Block that we place the VOP in.
  ;;  3] This structure.
  ;;  4] Head of argument TN-Ref list.
  ;;  5] Head of result TN-Ref list.
  ;;  6] If Info-Arg-Count is non-zero, then a list of the magic arguments.
  ;;
  ;; Two values are returned: the first and last VOP emitted.  This vop
  ;; sequence must be linked into the VOP Next/Prev chain for the block.  At
  ;; least one VOP is always emitted.
  (emit-function (required-argument) :type function))

(defprinter template
  name
  arg-types
  result-types
  (more-args-type :test more-args-type :prin1 more-args-type)
  (more-results-type :test more-results-type :prin1 more-results-type)
  policy
  cost
  (note :test note)
  (info-arg-count :test (not (zerop info-arg-count))))

;;; The VOP-Info structure holds the constant information for a given virtual
;;; operation.  We include Template so functions with a direct VOP equivalent
;;; can be translated easily.
;;;
(defstruct (vop-info
	    (:include template)
	    (:print-function %print-template)
	    (:make-load-form-fun :ignore-it))
  ;;
  ;; Side-effects of this VOP and side-effects that affect the value of this
  ;; VOP.
  (effects (required-argument) :type attributes)
  (affected (required-argument) :type attributes)
  ;;
  ;; If true, causes special casing of TNs live after this VOP that aren't
  ;; results:
  ;; -- If T, all such TNs that are allocated in a SC with a defined save-sc
  ;;    will be saved in a TN in the save SC before the VOP and restored after
  ;;    the VOP.  This is used by call VOPs.  A bit vector representing the
  ;;    live TNs is stored in the VOP-SAVE-SET.
  ;; -- If :Force-To-Stack, all such TNs will made into :Environment TNs and
  ;;    forced to be allocated in SCs without any save-sc.  This is used by NLX
  ;;    entry vops.
  ;; -- If :Compute-Only, just compute the save set, don't do any saving.  This
  ;;    is used to get the live variables for debug info.
  ;;
  (save-p nil :type (member t nil :force-to-stack :compute-only))
  ;;
  ;; Info for automatic emission of move-arg VOPs by representation selection.
  ;; If NIL, then do nothing special.  If non-null, then there must be a more
  ;; arg.  Each more arg is moved to its passing location using the appropriate
  ;; representation-specific move-argument VOP.  The first (fixed) argument
  ;; must be the control-stack frame pointer for the frame to move into.  The
  ;; first info arg is the list of passing locations.
  ;;
  ;; Additional constraints depend on the value:
  ;;
  ;; :FULL-CALL
  ;;     None.
  ;;
  ;; :LOCAL-CALL
  ;;     The second (fixed) arg is the NFP for the called function (from
  ;;     ALLOCATE-FRAME.)
  ;;
  ;; :KNOWN-RETURN
  ;;     If needed, the old NFP is computed using COMPUTE-OLD-NFP.
  ;;
  (move-args nil :type (member nil :full-call :local-call :known-return))
  ;;
  ;; A list of sc-vectors representing the loading costs of each fixed argument
  ;; and result.
  (arg-costs nil :type list)
  (result-costs nil :type list)
  ;;
  ;; If true, sc-vectors representing the loading costs for any more args and
  ;; results.
  (more-arg-costs nil :type (or sc-vector null))
  (more-result-costs nil :type (or sc-vector null))
  ;;
  ;; Lists of sc-vectors mapping each SC to the SCs that we can load into.  If
  ;; a SC is directly acceptable to the VOP, then the entry is T.  Otherwise,
  ;; it is a list of the SC numbers of all the SCs that we can load into.  This
  ;; list will be empty if there is no load function which loads from that SC
  ;; to an SC allowed by the operand SC restriction.
  (arg-load-scs nil :type list)
  (result-load-scs nil :type list)
  ;;
  ;; If true, a function that is called with the VOP to do operand targeting.
  ;; This is done by modifiying the TN-Ref-Target slots in the TN-Refs so that
  ;; they point to other TN-Refs in the same VOP.
  (target-function nil :type (or null function))
  ;;
  ;; A function that emits assembly code for a use of this VOP when it is
  ;; called with the VOP structure.  Null if this VOP has no specified
  ;; generator (i.e. it exists only to be inherited by other VOPs.)
  (generator-function nil :type (or function null))
  ;;
  ;; A list of things that are used to parameterize an inherited generator.
  ;; This allows the same generator function to be used for a group of VOPs
  ;; with similar implementations.
  (variant nil :type list)
  ;;
  ;; The number of arguments and results.  Each regular arg/result counts as
  ;; one, and all the more args/results together count as 1.
  (num-args 0 :type index)
  (num-results 0 :type index)
  ;;
  ;; Vector of the temporaries the vop needs.  See emit-generic-vop in vmdef
  ;; for information on how the temps are encoded.
  (temps nil :type (or null (simple-array (unsigned-byte 16) (*))))
  ;;
  ;; The order all the refs for this vop should be put in.  Each operand is
  ;; assigned a number in the following ordering:
  ;;  args, more-args, results, more-results, temps
  ;; This vector represents the order the operands should be put into in the
  ;; next-ref link.
  (ref-ordering nil :type (or null (simple-array (unsigned-byte 8) (*))))
  ;;
  ;; Array of the various targets that should be done.  Each element encodes
  ;; the source ref (shifted 8) and the dest ref index.
  (targets nil :type (or null (simple-array (unsigned-byte 16) (*)))))


#[ Storage Bases and Classes

New interface: instead of CURRENT-FRAME-SIZE, have CURRENT-SB-SIZE <name> which
returns the current element size of the named SB.

How can we have primitive types that overlap, i.e. (UNSIGNED-BYTE 32),
(SIGNED-BYTE 32), FIXNUM?
Primitive types are used for two things:
    Representation selection: which SCs can be used to represent this value?
	For this purpose, it isn't necessary that primitive types be disjoint,
	since any primitive type can choose an arbitrary set of
	representations.  For moves between the overlapping representations,
	the move/load operations can just be noops when the locations are the
	same (vanilla MOVE), since any bad moves should be caught out by type
	checking.
    VOP selection:
	Is this operand legal for this VOP?  When ptypes overlap in interesting
	ways, there is a problem with allowing just a simple ptype restriction,
	since we might want to allow multiple ptypes.  This could be handled
	by allowing "union primitive types", or by allowing multiple primitive
	types to be specified (only in the operand restriction.)  The latter
	would be long the lines of other more flexible VOP operand restriction
	mechanisms, (constant, etc.)



Ensure that load/save-operand never need to do representation conversion.

The PRIMITIVE-TYPE more/coerce info would be moved into the SC.  This could
perhaps go along with flushing the TN-COSTS.  We would annotate the TN with
best SC, which implies the representation (boxed or unboxed).  We would still
need represent the legal SCs for restricted TNs somehow, and also would have to
come up with some other way for pack to keep track of which SCs we have already
tried.

A SC would have a list of "alternate" SCs and a boolean SAVE-P value that
indicates it needs to be saved across calls in some non-SAVE-P SC.  A TN is
initially given its "best" SC.  The SC is annotated with VOPs that are used for
moving between the SC and its alternate SCs (load/save operand, save/restore
register).  It is also annotated with the "move" VOPs used for moving between
this SC and all other SCs it is possible to move between.  We flush the idea
that there is only c-to-t and c-from-t.

But how does this mesh with the idea of putting operand load/save back into the
generator?  Maybe we should instead specify a load/save function?  The
load/save functions would also differ from the move VOPs in that they would
only be called when the TN is in fact in that particular alternate SC, whereas
the move VOPs will be associated with the primary SC, and will be emitted
before it is known whether the TN will be packed in the primary SC or an
alternate.

I guess a packed SC could also have immediate SCs as alternate SCs, and
constant loading functions could be associated with SCs using this mechanism.

So given a TN packed in SC X and a SC restriction for Y and Z, how do we know
which load function to call?  There would be ambiguity if X was an alternate
for both Y and Z and they specified different load functions.  This seems
unlikely to arise in practice, though, so we could just detect the ambiguity
and give an error at define-vop time.  If they are doing something totally
weird, they can always inhibit loading and roll their own.

Note that loading costs can be specified at the same time (same syntax) as
association of loading functions with SCs.  It seems that maybe we will be
rolling DEFINE-SAVE-SCS and DEFINE-MOVE-COSTS into DEFINE-STORAGE-CLASS.

Fortunately, these changes will affect most VOP definitions very little.


A Storage Base represents a physical storage resource such as a register set or
stack frame.  Storage bases for non-global resources such as the stack are
relativized by the environment that the TN is allocated in.  Packing conflict
information is kept in the storage base, but non-packed storage resources such
as closure environments also have storage bases.
Some storage bases:
    General purpose registers
    Floating point registers
    Boxed (control) stack environment
    Unboxed (number) stack environment
    Closure environment

A storage class is a potentially arbitrary set of the elements in a storage
base.  Although conceptually there may be a hierarchy of storage classes such
as "all registers", "boxed registers", "boxed scratch registers", this doesn't
exist at the implementation level.  Such things can be done by specifying
storage classes whose locations overlap.  A TN shouldn't have lots of
overlapping SC's as legal SC's, since time would be wasted repeatedly
attempting to pack in the same locations.

There will be some SC's whose locations overlap a great deal, since we get Pack
to do our representation analysis by having lots of SC's.  A SC is basically a
way of looking at a storage resource.  Although we could keep a fixnum and an
unboxed representation of the same number in the same register, they correspond
to different SC's since they are different representation choices.

TNs are annotated with the primitive type of the object that they hold:
    T: random boxed object with only one representation.
    Fixnum, Integer, XXX-Float: Object is always of the specified numeric type.
    String-Char: Object is always a string-char.

When a TN is packed, it is annotated with the SC it was packed into.  The code
generator for a VOP must be able to uniquely determine the representation of
its operands from the SC. (debugger also...)

Some SCs:
    Reg: any register (immediate objects)
    Save-Reg: a boxed register near r15 (registers easily saved in a call)
    Boxed-Reg: any boxed register (any boxed object)
    Unboxed-Reg: any unboxed register (any unboxed object)
    Float-Reg, Double-Float-Reg: float in FP register.
    Stack: boxed object on the stack (on cstack)
    Word: any 32bit unboxed object on nstack.
    Double: any 64bit unboxed object on nstack.

We have a number of non-packed storage classes which serve to represent access
costs associated with values that are not allocated using conflicts
information.  Non-packed TNs appear to already be packed in the appropriate
storage base so that Pack doesn't get confused.  Costs for relevant non-packed
SC's appear in the TN-Ref cost information, but need not ever be summed into
the TN cost vectors, since TNs cannot be packed into them.

There are SCs for non-immediate constants and for each significant kind of
immediate operand in the architecture.  On the RT, 4, 8 and 20 bit integer SCs
are probably worth having.

Non-packed SCs:
    Constant
    Immediate constant SCs:
        Signed-Byte-<N>, Unsigned-Byte-<N>, for various architecture dependent
	    values of <N>
	String-Char
	XXX-Float
	Magic values: T, NIL, 0.
]#


;;;; SBs and SCs.

(eval-when (compile load eval)

;;; Storage Base.  The SB structure represents the global information
;;; associated with a storage base.
;;;
(defstruct (sb
	    (:print-function %print-sb)
	    (:make-load-form-fun :just-dump-it-normally))
  ;;
  ;; Name, for printing and reference.
  (name nil :type symbol)
  ;;
  ;; The kind of storage base (which determines the packing algorithm).
  (kind :non-packed :type (member :finite :unbounded :non-packed))
  ;;
  ;; The number of elements in the SB.  If finite, this is the total size.  If
  ;; unbounded, this is the size that the SB is initially allocated at.
  (size 0 :type index))

(defprinter sb
  name)

;;; The Finite-SB structure holds information needed by the packing algorithm
;;; for finite SBs.
;;;
(defstruct (finite-sb (:include sb)
		      (:print-function %print-sb))
  ;;
  ;;
  ;; The number of locations currently allocated in this SB.
  (current-size 0 :type index)
  ;;
  ;; The last location packed in, used by pack to scatter TNs to prevent a few
  ;; locations from getting all the TNs, and thus getting overcrowded, reducing
  ;; the possiblilities for targeting.
  (last-offset 0 :type index)
  ;;
  ;; A vector containing, for each location in this SB, a vector indexed by IR2
  ;; block numbers, holding local conflict bit vectors.  A TN must not be
  ;; packed in a given location within a particular block if the LTN number for
  ;; that TN in that block corresponds to a set bit in the bit-vector.
  (conflicts '#() :type simple-vector)
  ;;
  ;; A vector containing, for each location in this SB, a bit-vector indexed by
  ;; IR2 block numbers.  If the bit corresponding to a block is set, then the
  ;; location is in use somewhere in the block, and thus has a conflict for
  ;; always-live TNs.
  (always-live '#() :type simple-vector)
  ;;
  ;; A vector containing the TN currently live in each location in the SB, or
  ;; NIL if the location is unused.  This is used during load-tn pack.
  (live-tns '#() :type simple-vector)
  ;;
  ;; The number of blocks for which the ALWAYS-LIVE and CONFLICTS might not be
  ;; virgin, and thus must be reinitialized when PACK starts.  Less then the
  ;; length of those vectors when not all of the length was used on the
  ;; previously packed component.
  (last-block-count 0 :type index))

;;; Storage Class.  The SC structure holds the storage base that storage is
;;; allocated in and information used to select locations within the SB.
;;;
(defstruct (sc (:print-function %print-sc))
  ;;
  ;; Name, for printing and reference.
  (name nil :type symbol)
  ;;
  ;; The number used to index SC cost vectors.
  (number 0 :type sc-number)
  ;;
  ;; The storage base that this SC allocates storage from.
  (sb nil :type (or sb null))
  ;;
  ;; The size of elements in this SC, in units of locations in the SB.
  (element-size 0 :type index)
  ;;
  ;; If our SB is finite, a list of the locations in this SC.
  (locations nil :type list)
  ;;
  ;; A list of the alternate (save) SCs for this SC.
  (alternate-scs nil :type list)
  ;;
  ;; A list of the constant SCs that can me moved into this SC.
  (constant-scs nil :type list)
  ;;
  ;; True if this values in this SC needs to be saved across calls.
  (save-p nil :type boolean)
  ;;
  ;; Vectors mapping from SC numbers to information about how to load from the
  ;; index SC to this one.  Move-Functions holds the names of the functions
  ;; used to do loading, and Load-Costs holds the cost of the corresponding
  ;; Move-Functions.  If loading is impossible, then the entries are NIL.
  ;; Load-Costs is initialized to have a 0 for this SC.
  (move-functions (make-array sc-number-limit :initial-element nil)
		  :type sc-vector)
  (load-costs (make-array sc-number-limit :initial-element nil)
	      :type sc-vector)
  ;;
  ;; Vector mapping from SC numbers to possibly representation-specific move
  ;; and coerce VOPs.  Each entry is a list of VOP-INFOs for VOPs that
  ;; move/coerce an object in the index SC's representation into this SC's
  ;; representation.  This vector is filled out with entries for all SCs that
  ;; can somehow be coerced into this SC, not just those VOPs defined to
  ;; directly move into this SC (i.e. it allows for operand loading on the move
  ;; VOP's operands.)
  ;;
  ;; When there are multiple applicable VOPs, the template arg and result type
  ;; restrictions are used to determine which one to use.  The list is sorted
  ;; by increasing cost, so the first applicable VOP should be used.
  ;;
  ;; Move (or move-arg) VOPs with descriptor results shouldn't have TNs wired
  ;; in the standard argument registers, since there may already be live TNs
  ;; wired in those locations holding the values that we are setting up for
  ;; unknown-values return.
  (move-vops (make-array sc-number-limit :initial-element nil)
	     :type sc-vector)
  ;;
  ;; The costs corresponding to the MOVE-VOPS.  Separate because this info is
  ;; needed at meta-compile time, while the MOVE-VOPs don't exist till load
  ;; time.  If no move is defined, then the entry is NIL.
  (move-costs (make-array sc-number-limit :initial-element nil)
	      :type sc-vector)
  ;;
  ;; Similar to Move-VOPs, except that we only ever use the entries for this SC
  ;; and its alternates, since we never combine complex representation
  ;; conversion with argument passing.
  (move-arg-vops (make-array sc-number-limit :initial-element nil)
		 :type sc-vector)
  ;;
  ;; True if this SC or one of its alternates in in the NUMBER-STACK SB.
  (number-stack-p nil :type boolean)
  ;;
  ;; Alignment restriction.  The offset must be an even multiple of this.
  (alignment 1 :type (and index (integer 1)))
  ;;
  ;; A list of locations that we avoid packing in during normal register
  ;; allocation to ensure that these locations will be free for operand
  ;; loading.  This prevents load-TN packing from thrashing by spilling a lot.
  (reserve-locations nil :type list))

(defprinter sc
  name)

); eval-when (compile load eval)


;;;; TNs.

(defstruct (tn (:include sset-element)
	       (:constructor make-random-tn)
	       (:constructor really-make-tn (number kind primitive-type sc))
	       (:print-function %print-tn))
  ;;
  ;; The kind of TN this is:
  ;;
  ;;   :Normal
  ;;        A normal, non-constant TN, representing a variable or temporary.
  ;;        Lifetime information is computed so that packing can be done.
  ;;
  ;;   :Environment
  ;;        A TN that has hidden references (debugger or NLX), and thus must be
  ;;        allocated for the duration of the environment it is referenced in.
  ;;
  ;;   :DEBUG-ENVIRONMENT
  ;;        Like :ENVIRONMENT, but is used for TNs that we want to be able to
  ;;        target to/from and that don't absolutely have to be live
  ;;        everywhere.  These TNs are live in all blocks in the environment
  ;;        that don't reference this TN.
  ;;
  ;;   :Component
  ;;        A TN that implicitly conflicts with all other TNs.  No conflict
  ;;        info is computed.
  ;;
  ;;   :Save
  ;;   :Save-Once
  ;;        A TN used for saving a :Normal TN across function calls.  The
  ;;        lifetime information slots are unitialized: get the original TN our
  ;;        of the SAVE-TN slot and use it for conflicts. Save-Once is like
  ;;        :Save, except that it is only save once at the single writer of the
  ;;        original TN.
  ;;
  ;;   :SPECIFIED-SAVE
  ;;        A TN that was explicitly specified as the save TN for another TN.
  ;;        When we actually get around to doing the saving, this will be
  ;;        changed to :SAVE or :SAVE-ONCE.
  ;;
  ;;   :Load
  ;;        A load-TN used to compute an argument or result that is restricted
  ;;        to some finite SB.  Load TNs don't have any conflict information.
  ;;        Load TN pack uses a special local conflict determination method.
  ;;
  ;;   :Constant
  ;;        Represents a constant, with TN-Leaf a Constant leaf.  Lifetime
  ;;        information isn't computed, since the value isn't allocated by
  ;;        pack, but is instead generated as a load at each use.  Since
  ;;        lifetime analysis isn't done on :Constant TNs, they don't have
  ;;        Local-Numbers and similar stuff.
  ;;
  ;;   :ALIAS
  ;;        A special kind of TN used to represent initialization of local call
  ;;        arguments in the caller.  It provides another name for the argument
  ;;        TN so that lifetime analysis doesn't get confused by self-recursive
  ;;        calls.  Lifetime analysis treats this the same as :NORMAL, but then
  ;;        at the end merges the conflict info into the original TN and
  ;;        replaces all uses of the alias with the original TN.  SAVE-TN holds
  ;;        the aliased TN.
  ;;
  (kind (required-argument)
	:type (member :normal :environment :debug-environment
		      :save :save-once :specified-save :load :constant
		      :component :alias))
  ;;
  ;; The primitive-type for this TN's value.  Null in restricted or wired TNs.
  (primitive-type nil :type (or primitive-type null))
  ;;
  ;; If this TN represents a variable or constant, then this is the
  ;; corresponding Leaf.
  (leaf nil :type (or leaf null))
  ;;
  ;; Thread that links TNs together so that we can find them.
  (next nil :type (or tn null))
  ;;
  ;; Head of TN-Ref lists for reads and writes of this TN.
  (reads nil :type (or tn-ref null))
  (writes nil :type (or tn-ref null))
  ;;
  ;; A link we use when building various temporary TN lists.
  (next* nil :type (or tn null))
  ;;
  ;; Some block that contains a reference to this TN, or Nil if we haven't seen
  ;; any reference yet.  If the TN is local, then this is the block it is local
  ;; to.
  (local nil :type (or ir2-block null))
  ;;
  ;; If a local TN, the block relative number for this TN.  Global TNs whose
  ;; liveness changes within a block are also assigned a local number during
  ;; the conflicts analysis of that block.  If the TN has no local number
  ;; within the block, then this is Nil.
  (local-number nil :type (or local-tn-number null))
  ;;
  ;; If a local TN, a bit-vector with 1 for the local-number of every TN that
  ;; we conflict with.
  (local-conflicts (make-array local-tn-limit :element-type 'bit
			       :initial-element 0)
		   :type local-tn-bit-vector)
  ;;
  ;; Head of the list of Global-Conflicts structures for a global TN.  This
  ;; list is sorted by block number (i.e. reverse DFO), allowing the
  ;; intersection between the lifetimes for two global TNs to be easily found.
  ;; If null, then this TN is a local TN.
  (global-conflicts nil :type (or global-conflicts null))
  ;;
  ;; During lifetime analysis, this is used as a pointer into the conflicts
  ;; chain, for scanning through blocks in reverse DFO.
  (current-conflict nil)
  ;;
  ;; In a :Save TN, this is the TN saved.  In a :Normal or :Environment TN,
  ;; this is the associated save TN.  In TNs with no save TN, this is null.
  (save-tn nil :type (or tn null))
  ;;
  ;; After pack, the SC we packed into.  Beforehand, the SC we want to pack
  ;; into, or null if we don't know.
  (sc nil :type (or sc null))
  ;;
  ;; The offset within the SB that this TN is packed into.  This is what
  ;; indicates that the TN is packed.
  (offset nil :type (or index null))
  ;;
  ;; Some kind of info about how important this TN is.
  (cost 0 :type fixnum)
  ;;
  ;; If a :ENVIRONMENT or :DEBUG-ENVIRONMENT TN, this is the environment that
  ;; the TN is live throughout.
  (environment nil :type (or environment null)))

(defun %print-tn (s stream d)
  (declare (ignore d))
  (write-string "#<TN " stream)
  (print-tn s stream)
  (write-char #\> stream))

;;; The Global-Conflicts structure represents the conflicts for global TNs.
;;; Each global TN has a list of these structures, one for each block that it
;;; is live in.  In addition to repsenting the result of lifetime analysis, the
;;; global conflicts structure is used during lifetime analysis to represent
;;; the set of TNs live at the start of the IR2 block.
;;;
(defstruct (global-conflicts
	    (:constructor really-make-global-conflicts (kind tn block number))
	    (:print-function %print-global-conflicts))

  ;;
  ;; The IR2-Block that this structure represents the conflicts for.
  (block (required-argument) :type ir2-block)
  ;;
  ;; Thread running through all the Global-Conflict for Block.  This
  ;; thread is sorted by TN number.
  (next nil :type (or global-conflicts null))
  ;;
  ;; The way that TN is used by Block:
  ;;
  ;;    :Read
  ;;        The TN is read before it is written.  It starts the block live, but
  ;;        is written within the block.
  ;;
  ;;    :Write
  ;;        The TN is written before any read.  It starts the block dead, and
  ;;        need not have a read within the block.
  ;;
  ;;    :Read-Only
  ;;        The TN is read, but never written.  It starts the block live, and
  ;;        is not killed by the block.  Lifetime analysis will promote
  ;;        :Read-Only TNs to :Live if they are live at the block end.
  ;;
  ;;    :Live
  ;;        The TN is not referenced.  It is live everywhere in the block.
  ;;
  (kind :read-only :type (member :read :write :read-only :live))
  ;;
  ;; A local conflicts vector representing conflicts with TNs live in Block.
  ;; The index for the local TN number of each TN we conflict with in this
  ;; block is 1.  To find the full conflict set, the :Live TNs for Block must
  ;; also be included.  This slot is not meaningful when Kind is :Live.
  (conflicts (make-array local-tn-limit
			 :element-type 'bit
			 :initial-element 0)
	     :type local-tn-bit-vector)
  ;;
  ;; The TN we are recording conflicts for.
  (tn (required-argument) :type tn)
  ;;
  ;; Thread through all the Global-Conflicts for TN.
  (tn-next nil :type (or global-conflicts null))
  ;;
  ;; TN's local TN number in Block.  :Live TNs don't have local numbers.
  (number nil :type (or local-tn-number null)))

(defprinter global-conflicts
  tn
  block
  kind
  (number :test number))
