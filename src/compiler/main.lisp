;;; Top-level interfaces to the compiler.

(in-package "C")

(in-package "EXTENSIONS")
(export '(*compile-progress* compile-from-stream *block-compile-default*
			     start-block end-block
			     *byte-compile-default*
			     *byte-compile-top-level*))

(in-package "LISP")
(export '(*compile-verbose* *compile-print* *compile-file-pathname*
			    *compile-file-truename*
			    compile-file-pathname))

(in-package "C")

(proclaim '(special *constants* *free-variables* *compile-component*
		    *code-vector* *next-location* *result-fixups*
		    *free-functions* *source-paths*
		    *continuation-number* *continuation-numbers*
		    *number-continuations* *tn-id* *tn-ids* *id-tns*
		    *label-ids* *label-id* *id-labels*
		    *undefined-warnings* *compiler-error-count*
		    *compiler-warning-count* *compiler-note-count*
		    *compiler-error-output* *compiler-error-bailout*
		    *compiler-trace-output*
		    *last-source-context* *last-original-source*
		    *last-source-form* *last-format-string* *last-format-args*
		    *last-message-count* *lexical-environment*
		    *coalesce-constants*))

;; FIX consider a package for each phase

#[ Compiler Overview

  FIX where are components,environments introduced?

  FIX where is the reference to very common code
         eg do-blocks from macros.lisp
                maybe with the introduction of blocks?
                maybe with representations?

The structure of the compiler may be broadly characterized by describing
the compilation phases and the data structures that they manipulate.  The
steps in the compilation are called phases rather than passes since they
don't necessarily involve a full pass over the code.  The data used to
represent the code at some point is called an "intermediate
representation".

In the text below, the source files which primarily contain each part are
listed after "Files: ".

Two major intermediate representations are used in the compiler:

  % ICR

    The [Implicit Continuation Representation] (ICR) represents the
    lisp-level semantics of the source code during the initial phases.
    Partial evaluation and semantic analysis are done on this
    representation.  ICR is roughly equivalent to a subset of the language
    as a whole, but is represented as a flow-graph rather than a syntax
    tree.  Phases which only manipulate ICR comprise the "front end".  It
    would be possible to use a different back end such as one that directly
    generated code for a stack machine.  Files: node

  % VMR

    The [Virtual Machine Representation] (VMR) represents the
    implementation of the source code on a virtual machine.  The virtual
    machine may vary depending on the the target hardware, but VMR is
    sufficiently stylized that most of the phases which manipulate it are
    portable.  Files: vop

Each phase is briefly described here.  The phases from "local call
analysis" to "type constraint propagation" all interact; for maximum
optimization, they are generally repeated until nothing new is discovered.

[Compiler Interface] details the public interface to the compiler.  Files:
main

== Front ==

  % [ICR conversion]

    Convert the source into ICR, doing macroexpansion and simple
    source-to-source transformation.  All names are resolved at this time,
    so we don't have to worry about name conflicts later on.  Files:
    ir1tran, srctran, typetran

  % [Local call analysis]

    Find calls to local functions and convert them to local calls to the
    correct entry point, doing keyword parsing, etc.  Recognize once-called
    functions as `let's.  Create "external entry points" for entry-point
    functions.  Files: locall

  % [Find components]

    Find flow graph components and compute depth-first ordering.  Separate
    top-level code from run-time code, and determine which components are
    top-level components.  Files: dfo

  % [ICR optimize]

    A grab-bag of all the non-flow ICR optimizations.  Fold constant
    functions, propagate types and eliminate code that computes unused
    values.  Special-case calls to some known global functions by replacing
    them with a computed function.  Merge blocks and eliminate `if'-`if's.
    Substitute `let' variables.  Files: ir1opt, ir1tran, typetran, seqtran,
    FIX? vm/vm-tran

  % [Type propagation]

    Use global flow analysis to propagate information about lexical
    variable types.  Eliminate redundant type checks and tests.  Files:
    constraint

  % [Type check generation]

    Emit explicit ICR code for any necessary type checks that are too
    complex to be easily generated on the fly by the back end.  Files:
    checkgen

  % [Event driven operations]

    Various parts of ICR are incrementally recomputed, either eagerly on
    modification of the ICR, or lazily, when the relevant information is
    needed.

      * Check that type assertions are satisfied, marking places where type
        checks need to be done.

      * Locate `let' calls.

      * Delete functions and variables with no references

    Files: ir1util, ir1opt

  % [ICR finalize]

    This phase is run after all components have been compiled.  It scans
    the global variable references, looking for references to undefined
    variables and incompatible function redefinitions.  Files: ir1final,
    main.

  % [Environment analysis]

    Determine which distinct environments need to be allocated, and what
    context needed to be closed over by each environment.  We detect
    non-local exits and set closure variables.  We also emit cleanup code
    as funny function calls.  This is the last pure ICR pass.  Files:
    envanal

== Middle ==

  % [Global TN Assignment]

    Iterate over all defined functions, determining calling conventions and
    assigning Temporary Names (TNs) to local variables.  Files: gtn

  % [Local TN Assignment]

    Use type and policy information to determine which VMR translation to
    use for known functions, and then create TNs for expression evaluation
    temporaries.  We also accumulate some random information needed by VMR
    conversion.  Files: ltn

  % [Control analysis]

    Linearize the flow graph in a way that minimizes the number of
    branches.  The block-level structure of the flow graph is basically
    frozen at this point.  Files: control

  % [Stack analysis]

    Maintain stack discipline for unknown-values continuation in the
    presence of local exits.  Files: stack

  % [Entry analysis]

    Collect some back-end information for each externally callable
    function.  Files: entry

  % [VMR conversion]

    Convert ICR into VMR by translating nodes into VOPs.  Emit type checks.
    Files: ir2tran, vmdef

== Back ==

  % [Copy propagation]

    Use flow analysis to eliminate unnecessary copying of TN values.
    Files: copyprop

  % [Representation selection]

    Look at all references to each TN to determine which representation has
    the lowest cost.  Emit appropriate move and coerce VOPS for that
    representation.  Files: represent

  % [Lifetime analysis]

    Do flow analysis to find the set of TNs whose lifetimes overlap with
    the lifetimes of each TN being packed.  Annotate call VOPs with the TNs
    that need to be saved.  Files: life

  % [Packing]

    Find a legal register allocation, attempting to minimize unnecessary
    moves.  Files: pack

  % [Code generation]

    Call the VOP generators to emit assembly code.  Files: codegen

  % [Pipeline reorganization]

    On some machines, move memory references backward in the code so that
    they can overlap with computation.  On machines with delayed branch
    instructions, locate instructions that can be moved into delay slots.
    Files: new-assem

  % [Assembly]

    Resolve branches and convert in to object code and fixup information.
    Files: new-assem

  % [Dumping]

    Convert the compiled code into an object file or in-core function.
    Files: debug-dump, dump, generic/core
]#

#[ Implicit Continuation Representation

Files: node

FIX where is leaf introduced?
FIX where are lambdas introduced?

The set of special forms recognized is exactly that specified in FIX the
Common Lisp manual.  Everything that is described as a macro in FIX CLTL is
a macro.

Large amounts of syntactic information are thrown away by the conversion to
an anonymous flow graph representation.  The elimination of names
eliminates the need to represent most environment manipulation special
forms.  The explicit representation of control eliminates the need to
represent `block' and `go', and makes flow analysis easy.  A full `lambda'
is implemented with a simple fixed-arg lambda, which greatly simplifies
later code.

The elimination of syntactic information eliminates the need for most of the
"beta transformation" optimizations in Rabbit.  There are no progns, no
tagbodys and no returns.  There are no "close parens" which get in the way of
determining which node receives a given value.

In ICR, computation is represented by Nodes.  The node types are defined in
c:node.lisp.  These are the nine node types:

  % if

    Represents all conditionals.

  % set

    Represents a `setq'.

  % ref

    Represents a constant or variable reference.

  % combination

    Represents a normal function call.

  % MV-combination

    Represents a `multiple-value-call'.  This is used to implement all
    multiple value receiving forms except for `multiple-value-prog1', which
    is implicit.

  % bind

    This represents the allocation and initialization of the variables in a
    lambda.

  % return

    This collects the return value from a lambda and represents the control
    transfer on return.

  % entry

    Marks the start of a dynamic extent that can have non-local exits to
    it.  Dynamic state can be saved at this point for restoration on
    re-entry.

  % exit

    Marks a potentially non-local exit.  This node is interposed between
    the non-local uses of a continuation and the dest so that code to do a
    non-local exit can be inserted if necessary.

Some slots are shared between all node types (via `defstruct' inheritance.)
This information held in common between all nodes often makes it possible
to avoid special-casing nodes on the basis of type.  This shared
information is primarily concerned with the order of evaluation and
destinations and properties of results.  This control and value flow is
indicated in the node primarily by pointing to continuations.

The {\tt continuation} structure represents information sufficiently
related to the normal notion of a continuation that naming it so seems
sensible.  Basically, a continuation represents a place in the code, or
alternatively the destination of an expression result and a transfer of
control.  These two notions are bound together for the same reasons that
they are related in the standard functional continuation interpretation.

A continuation may be deprived of either or both of its value or control
significance.  If the value of a continuation is unused due to evaluation
for effect, then the continuation will have a null {\tt dest}.  If the {\tt
next} node for a continuation is deleted by some optimization, then {\tt
next} will be {\tt :none}.

  [XXX Continuation kinds...]

The {\tt block} structure represents a basic block, in the the normal
sense.  Control transfers other than simple sequencing are represented by
information in the block structure.  The continuation for the last node in
a block represents only the destination for the result.

It is very difficult to reconstruct anything resembling the original source
from ICR, so we record the original source form in each node.  The location
of the source form within the input is also recorded, allowing for
interfaces such as "Edit Compiler Warnings".  See section
\ref{source-paths}.

Forms such as special-bind and catch need to have cleanup code executed at
all exit points from the form.  We represent this constraint in ICR by
annotating the code syntactically within the form with a Cleanup structure
describing what needs to be cleaned up.  [Environment analysis] determines
the cleanup locations by watching for a change in the cleanup between two
continuations.  We can't emit cleanup code during ICR conversion, since we
don't know which exits will be local until after ICR optimizations are
done.

Special binding is represented by a call to the funny function
`%special-bind'.  The first argument is the Global-Var structure for the
variable bound and the second argument is the value to bind it to.

Some subprimitives are implemented using a macro-like mechanism for
translating `%primitive' forms into arbitrary lisp code.  Subprimitives
special-cased by VMR conversion are represented by a call to the funny
function %%Primitive.  The corresponding Template structure is passed as      ; FIX first mention template
the first argument.

We check global function calls for syntactic legality with respect to any
defined function type function.  If the call is illegal or we are unable to
tell if it is legal due to non-constant keywords, then we give a warning
and mark the function reference as :notinline to force a full call and
cause subsequent phases to ignore the call.  If the call is legal and is to
a known function, then we annotate the Combination node with the
Function-Info structure that contains the compiler information for the
function.


== Tail sets ==
X |
Probably want to have a GTN-like function result equivalence class mechanism
for ICR type inference.  This would be like the return value propagation being
done by Propagate-From-Calls, but more powerful, less hackish, and known to
terminate.  The ICR equivalence classes could probably be used by GTN, as well.

What we do is have local call analysis eagerly maintain the equivalence classes
of functions that return the same way by annotating functions with a Tail-Info
structure shared between all functions whose value could be the value of this
function.  We don't require that the calls actually be tail-recursive, only
that the call deliver its value to the result continuation.  [XXX Actually
now done by ICR-OPTIMIZE-RETURN, which is currently making ICR optimize
mandatory.]

We can then use the Tail-Set during ICR type inference.  It would have a type
that is the union across all equivalent functions of the types of all the uses
other than in local calls.  This type would be recomputed during optimization
of return nodes.  When the type changes, we would propagate it to all calls to
any of the equivalent functions.  How do we know when and how to recompute the
type for a tail-set?  Recomputation is driven by type propagation on the result
continuation.

This is really special-casing of RETURN nodes.  The return node has the type
which is the union of all the non-call uses of the result.  The tail-set is
found though the lambda.  We can then recompute the overall union by taking the
union of the type per return node, rather than per-use.


How do result type assertions work?  We can't intersect the assertions across
all functions in the equivalence class, since some of the call combinations may
not happen (or even be possible).  We can intersect the assertion of the result
with the derived types for non-call uses.

When we do a tail call, we obviously can't check that the returned value
matches our assertion.  Although in principle, we would like to be able to
check all assertions, to preserve system integrity, we only need to check
assertions that we depend on.  We can afford to lose some assertion information
as long as we entirely lose it, ignoring it for type inference as well as for
type checking.

Things will work out, since the caller will see the tail-info type as the
derived type for the call, and will emit a type check if it needs a stronger
result.

A remaining question is whether we should intersect the assertion with
per-RETURN derived types from the very beginning (i.e. before the type check
pass).  I think the answer is yes.  We delay the type check pass so that we can
get our best guess for the derived type before we decide whether a check is
necessary.  But with the function return type, we aren't committing to doing
any type check when we intersect with the type assertion; the need to type
check is still determined in the type check pass by examination of the result
continuation.

What is the relationship between the per-RETURN types and the types in the
result continuation?  The assertion is exactly the Continuation-Asserted-Type
(note that the asserted type of result continuations will never change after
ICR conversion).  The per-RETURN derived type is different than the
Continuation-Derived-Type, since it is intersected with the asserted type even
before Type Check runs.  Ignoring the Continuation-Derived-Type probably makes
life simpler anyway, since this breaks the potential circularity of the
Tail-Info-Type will affecting the Continuation-Derived-Type, which affects...

When a given return has no non-call uses, we represent this by using
*empty-type*.  This consistent with the interpretation that a return type of
NIL means the function can't return.


== Hairy function representation ==

Non-fixed-arg functions are represented using Optional-Dispatch.  An
Optional-Dispatch has an entry-point function for each legal number of
optionals, and one for when extra args are present.  Each entry point function
is a simple lambda.  The entry point function for an optional is passed the
arguments which were actually supplied; the entry point function is expected to
default any remaining parameters and evaluate the actual function body.

If no supplied-p arg is present, then we can do this fairly easily by having
each entry point supply its default and call the next entry point, with the
last entry point containing the body.  If there are supplied-p args, then entry
point function is replaced with a function that calls the original entry
function with T's inserted at the position of all the supplied args with
supplied-p parameters.

We want to be a bit clever about how we handle arguments declared special when
doing optional defaulting, or we will emit really gross code for special
optionals.  If we bound the arg specially over the entire entry-point function,
then the entry point function would be caused to be non-tail-recursive.  What
we can do is only bind the variable specially around the evaluation of the
default, and then read the special and store the final value of the special
into a lexical variable which we then pass as the argument.  In the common case
where the default is a constant, we don't have to special-bind at all, since
the computation of the default is not affected by and cannot affect any special
bindings.

Keyword and rest args are both implemented using a LEXPR-like "more args"
convention.  The More-Entry takes two arguments in addition to the fixed and
optional arguments: the argument context and count.  (ARG <context> <n>)
accesses the N'th additional argument.  Keyword args are implemented directly
using this mechanism.  Rest args are created by calling %Listify-Rest-Args with
the context and count.

The More-Entry parses the keyword arguments and passes the values to the main
function as positional arguments.  If a keyword default is not constant, then
we pass a supplied-p parameter into the main entry and let it worry about
defaulting the argument.  Since the main entry accepts keywords in parsed form,
we can parse keywords at compile time for calls to known functions.  We keep
around the original parsed lambda-list and related information so that people
can figure out how to call the main entry.


== ICR representation of non-local exits ==

All exits are initially represented by EXIT nodes:
How about an Exit node:
    (defstruct (exit (:include node))
      value)
The Exit node uses the continuation that is to receive the thrown Value.
During optimization, if we discover that the Cont's home-lambda is the same is
the exit node's, then we can delete the Exit node, substituting the Cont for
all of the Value's uses.

The successor block of an EXIT is the entry block in the entered environment.
So we use the Exit node to mark the place where exit code is inserted.  During
environment analysis, we need only insert a single block containing the entry
point stub.

We ensure that all Exits that aren't for a NLX don't have any Value, so that
local exits never require any value massaging.

The Entry node marks the beginning of a block or tagbody:
    (defstruct (entry (:include node))
      (continuations nil :type list))

It contains a list of all the continuations that the body could exit to.  The
Entry node is used as a marker for the the place to snapshot state, including
the control stack pointer.  Each lambda has a list of its Entries so
that environment analysis can figure out which continuations are really being
closed over.  There is no reason for optimization to delete Entry nodes,
since they are harmless in the degenerate case: we just emit no code (like a
no-var let).


We represent CATCH using the lexical exit mechanism.  We do a transformation
like this:
   (catch 'foo xxx)  ==>
   (block #:foo
     (%catch X'(lambda () (return-from X:foo (%unknown-values))) 'foo)
     (%within-cleanup :catch
       xxx))

%CATCH just sets up the catch frame which points to the exit function.  %Catch
is an ordinary function as far as ICR is concerned.  The fact that the catcher
needs to be cleaned up is expressed by the Cleanup slots in the continuations
in the body.  %UNKNOWN-VALUES is a dummy function call which represents the
fact that we don't know what values will be thrown.

%WITHIN-CLEANUP is a special special form that instantiates its first argument
as the current cleanup when converting the body.  In reality, the lambda is
also created by the special special form %ESCAPE-FUNCTION, which gives the
lambda a special :ESCAPE kind so that the back end knows not to generate any
code for it.


We use a similar hack in Unwind-Protect to represent the fact that the cleanup
forms can be invoked at arbitrarily random times.
    (unwind-protect p c)  ==>
    (flet ((X:cleanup () c))
      (block X:return
        (multiple-value-bind
            (X:next X:start X:count)
            (block X:unwind
              (%unwind-protect X'(lambda (x) (return-from X:unwind x)))
              (%within-cleanup :unwind-protect
                (return-from X:return p)))
          (X:cleanup)
          (%continue-unwind X:next X:start X:count))))

We use the block X:unwind to represent the entry to cleanup code in the case
where we are non-locally unwound.  Calling of the cleanup function in the
drop-through case (or any local exit) is handled by cleanup generation.  We
make the cleanup a function so that cleanup generation can add calls at local
exits from the protected form.  X:next, X:start and X:count are state used in
the case where we are unwound.  They indicate where to go after doing the
cleanup and what values are being thrown.  The cleanup encloses only the
protected form.  As in CATCH, the escape function is specially tagged as
:ESCAPE.  The cleanup function is tagged as :CLEANUP to inhibit let conversion
(since references are added in environment analysis.)

Notice that implementing these forms using closures over continuations
eliminates any need to special-case ICR flow analysis.  Obviously we don't
really want to make heap-closures here.  In reality these functions are
special-cased by the back-end according to their KIND.


== Block compilation ==

One of the properties of ICR is that it supports "block compilation" by
allowing arbitrarily large amounts of code to be converted at once, with
actual compilation of the code being done at will.


In order to preserve the normal semantics we must recognize that proclamations
(possibly implicit) are scoped.  A proclamation is in effect only from the time
of appearance of the proclamation to the time it is contradicted.  The current
global environment at the end of a block is not necessarily the correct global
environment for compilation of all the code within the block.  We solve this
problem by closing over the relevant information in the ICR at the time it is
converted.  For example, each functional variable reference is marked as
inline, notinline or don't care.  Similarly, each node contains a structure
known as a Cookie which contains the appropriate settings of the compiler
policy switches.

We actually convert each form in the file separately, creating a separate
"initial component" for each one.  Later on, these components are merged as
needed.  The main reason for doing this is to cause EVAL-WHEN processing to be
interleaved with reading.

    FIX does the component mentioned here need introducing?


== Entry points ==

X|

Since we need to evaluate potentially arbitrary code in the XEP argument forms
(for type checking), we can't leave the arguments in the wired passing
locations.  Instead, it seems better to give the XEP max-args fixed arguments,
with the passing locations being the true passing locations.  Instead of using
%XEP-ARG, we reference the appropriate variable.

Also, it might be a good idea to do argument count checking and dispatching
with explicit conditional code in the XEP.  This would simplify both the code
that creates the XEP and the VMR conversion of XEPs.  Also, argument count
dispatching would automatically benefit from any cleverness in compilation of
case-like forms (jump tables, etc).  On the downside, this would push some
assumptions about how arg dispatching is done into ICR.  But then we are
currently violating abstraction at least as badly in VMR conversion, which is
also supposed to be implementation independent.
|X

As a side-effect of finding which references to known functions can be
converted to local calls, we find any references that cannot be converted.
References that cannot be converted to a local call must evaluate to a
"function object" (or function-entry) that can be called using the full call
convention.  A function that can be called from outside the component is called
an "entry-point".

Lots of stuff that happens at compile-time with local function calls must be
done at run-time when an entry-point is called.

It is desirable for optimization and other purposes if all the calls to every
function were directly present in ICR as local calls.  We cannot directly do
this with entry-point functions, since we don't know where and how the
entry-point will be called until run-time.

What we do is represent all the calls possible from outside the component by
local calls within the component.  For each entry-point function, we create a
corresponding lambda called the external entry point or XEP.  This is a
function which takes the number of arguments passed as the first argument,
followed by arguments corresponding to each required or optional argument.

If an optional argument is unsupplied, the value passed into the XEP is
undefined.  The XEP is responsible for doing argument count checking and
dispatching.

In the case of a fixed-arg lambda, we emit a call to the %VERIFY-ARGUMENT-COUNT
funny function (conditional on policy), then call the real function on the
passed arguments.  Even in this simple case, we benefit several ways from
having a separate XEP:
 -- The argument count checking is factored out, and only needs to be done in
    full calls.
 -- Argument type checking happens automatically as a consequence of passing
    the XEP arguments in a local call to the real function.  This type checking
    is also only done in full calls.
 -- The real function may use a non-standard calling convention for the benefit
    of recursive or block-compiled calls.  The XEP converts arguments/return
    values to/from the standard convention.  This also requires little
    special-casing of XEPs.

If the function has variable argument count (represented by an
OPTIONAL-DISPATCH), then the XEP contains a COND which dispatches off of the
argument count, calling the appropriate entry-point function (which then does
defaulting).  If there is a more entry (for keyword or rest args), then the XEP
obtains the more arg context and count by calling the %MORE-ARG-CONTEXT funny
function.

All non-local-call references to functions are replaced with references to the
corresponding XEP.  ICR optimization may discover a local call that was
previously a non-local reference.  When we delete the reference to the XEP, we
may find that it has no references.  In this case, we can delete the XEP,
causing the function to no longer be an entry-point.
]#

#[ Virtual Machine Representation

Files: vop

[FIX]
]#

#[ Compiler Interface

    The Lisp interface to the compiler.  Usage is detailed in [Calling the
    Compiler].

Files: c:main.lisp

Entry functions: `compile', `compile-file', `compile-from-stream', ...

Call sequences:

    compile
      ir1-top-level ; 1
        ir1-convert-lambda-body
          ir1-convert-special-bindings
            ir1-convert-aux-bindings
              ir1-convert-progn-body
                reference-constant
                ir1-convert
                  ir1-convert-variable
                    reference-leaf
                  reference-constant
                    maybe-emit-make-load-forms    FIX kinda special case?
                      emit-make-load-form
                        compile-top-level-lambdas
                          sub-compile-top-level-lambdas
                            compile-component
                        compile-load-time-value
                          compile-load-time-stuff
                            compile-top-level
                        compile-make-load-form-init-forms
                          compile-load-time-stuff
                            compile-top-level
                  reference-leaf
                  ir1-convert-global-functoid
      local-call-analyze ; 2
      find-initial-dfo   ; 3
      compile-component
	ir1-phases
	  ;; Front phases.
	  ir1-optimize-until-done
	    ir1-optimize          ; 4
	  local-call-analyze      ; 2
	  dfo-as-needed
	    find-dfo              ; 3
	  constraint-propagate    ; 5
	  generate-type-checks    ; 6
	  ir1-finalize            ; 8
        environment-analyze ; 9
        dfo-as-needed
        delete-if-no-entries
        native-compile-component
          ;; Middle phases.
          gtn-analyze     ; 10
          ltn-analyze     ; 11
          dfo-as-needed
          control-analyze ; 12
          stack-analyze   ; 13
          dfo-as-needed
          init-assembler
          entry-analyze   ; 14
          ir2-convert     ; 15
          ;; Back phases.
          FIX
        byte-compile-component
          FIX

    compile-file
      sub-compile-file
        finish-block-compilation
          compile-top-level
            ;; Front phases.
            ir1-optimize-until-done
              ir1-optimize          ; 4
            local-call-analyze      ; 2
            dfo-as-needed           ; 3
            constraint-propagate    ; 5
            generate-type-checks    ; 6
            ir1-finalize            ; 8
        process-sources
          process-form
            process-cold-load-form
              compile-top-level-lambdas
                sub-compile-top-level-lambdas
                  compile-component
            convert-and-maybe-compile
              ir1-top-level ; 1
              compile-top-level FIX vs above
                compile-top-level-lambdas
                  sub-compile-top-level-lambdas
                    compile-component
                compile-load-time-value-lambda
                  compile-component
                compile-load-time-value
                  compile-load-time-stuff
                    ir1-top-level
                    compile-top-level
            process-proclaim
              convert-and-maybe-compile
                compile-top-level
          process-progn
            process-form
          process-locally
            process-progn
      compile-top-level-lambdas
        sub-compile-top-level-lambdas
          compile-component

    compile-from-stream
      sub-compile-file
]#

#[ Compiler Organisation

[ Compiler Overview                    ]  A more detailed overview.

[ Implicit Continuation Representation ]  Intermediate Representation 1 (ICR)
[ Virtual Machine Representation       ]  Intermediate Representation 2 (VMR)

[ Compiler Interface                   ]  The Lisp interface to the compiler.

== Front ==

[ ICR conversion           ]   1 Read source, convert to IR1.
[ Local call analysis      ]   2 Convert to local call or let where possible.
[ Find components          ]   3 Split blocks into components, number blocks.
[ ICR optimize             ]   4 Join blocks, optimize specific to node type.
[ Type propagation         ]   5 Merge predecessor contraints into blocks.  Use to update node derived type.
[ Type check generation    ]   6
[ Event driven operations  ]   7
[ ICR finalize             ]   8
[ Environment analysis     ]   9

== Middle ==

[ Global TN assignment     ]  10
[ Local TN assignment      ]  11
[ Control analysis         ]  12
[ Stack analysis           ]  13
[ Entry analysis           ]  14
[ VMR conversion           ]  15

== Back ==

[ Copy propagation         ]  16
[ Representation selection ]  17
[ Lifetime analysis        ]  18
[ Packing                  ]  19
[ Code generation          ]  20
[ Pipeline Reorganization  ]  21
[ Assembly                 ]  22
[ Dumping                  ]  23

== User Interface ==

[ Error system interface  ]
[ Source tracking         ]
]#

;;; Exported:
(defvar *block-compile-default* :specified
  "The default value for the :block-compile argument to `compile-file' and
   `compile-from-stream'.")
(declaim (type (member t nil :specified) *block-compile-default*))

;;; Exported:
(defvar *byte-compile-default* :maybe
  "The fallback value for the :byte-compile argument to `compile-file' and
   `compile-from-stream'.")

;;; Exported:
(defvar *byte-compile-top-level* t
  "If this variable is #t and the byte-compile argument to `compile-file'
   or `compile-from-stream' is :maybe, then byte compile top-level code
   (evaluated at load-time, outside any `defun', etc).")

;;; Value of the :byte-compile argument to the compiler.
(defvar *byte-compile* :maybe)

;;; Bound by COMPILE-COMPONENT to T when byte-compiling, and NIL when
;;; native compiling.  During IR1 conversion this can also be :MAYBE, in which
;;; case we must look at the policy, see (byte-compiling).
;;;
(defvar *byte-compiling* :maybe)
(declaim (type (member t nil :maybe) *byte-compile* *byte-compiling*
	       *byte-compile-default*))

(defvar compiler-version "1.0")
(pushnew :python *features*)
(setf (getf ext:*herald-items* :python)
      `("    Python " ,compiler-version ", target "
	,#'(lambda (stream)
	     (write-string (backend-version *backend*) stream))))

(defvar *check-consistency* nil)
(defvar *all-components*)

;;; The current block compilation state.  These are initialized to the
;;; :Block-Compile and :Entry-Points arguments that COMPILE-FILE was called
;;; with.  Subsequent START-BLOCK or END-BLOCK declarations alter the values.
;;;
;;; *Block-Compile-Argument* holds the original value of the :block-compile
;;; argument, which overrides any internal declarations.
;;;
(defvar *block-compile*)
(defvar *block-compile-argument*)
(declaim (type (member nil t :specified)
	       *block-compile* *block-compile-argument*))
(defvar *entry-points*)
(declaim (list *entry-points*))

;;; When block compiling, used by PROCESS-FORM to accumulate top-level lambdas
;;; resulting from compiling subforms.  (In reverse order.)
;;;
(defvar *top-level-lambdas*)
(declaim (list *top-level-lambdas*))

(defvar *compile-verbose* t
  "The default for the :VERBOSE argument to `compile-file'.")
(defvar *compile-print* t
  "The default for the :PRINT argument to `compile-file'.")
(defvar *compile-progress* ()
  "The default for the :PROGRESS argument to `compile-file'.")

(defvar *compile-file-pathname* ()
  "The defaulted pathname of the file currently being compiled, or NIL if
   not compiling.")
(defvar *compile-file-truename* ()
  "The `truename' of the file currently being compiled, or NIL if not
   compiling.")

(declaim (type (or pathname null) *compile-file-pathname*
	       *compile-file-truename*))

;;; The values of *Package* and policy when compilation started.
;;;
(defvar *initial-package*)
(defvar *initial-cookie*)
(defvar *initial-interface-cookie*)

;;; The source-info structure for the current compilation.  This is null
;;; globally to indicate that we aren't currently in any identifiable
;;; compilation.
;;;
(defvar *source-info* nil)

;;; Maybe-Mumble  --  Internal
;;;
;;; Mumble conditional on *compile-progress*.
;;;
(defun maybe-mumble (&rest foo)
  (when *compile-progress*
    (apply #'compiler-mumble foo)))

;;; Maybe-Mumble-Verbose  --  Internal
;;;
;;; Mumble conditional on *compile-progress*.
;;;
(defun maybe-mumble-verbose (&rest foo)
  (when *compile-progress*
    (apply #'compiler-mumble foo)))

(deftype object () '(or fasl-file core-object null))

(defvar *compile-object* nil)
(declaim (type object *compile-object*))


;;;; Component compilation.

(defparameter max-optimize-iterations 6
  "The upper limit on the number of times that we will consecutively do IR1
  optimization that doesn't introduce any new code.  A finite limit is
  necessary, since type inference may take arbitrarily long to converge.")

(defevent ir1-optimize-until-done "IR1-OPTIMIZE-UNTIL-DONE called.")
(defevent ir1-optimize-maxed-out "Hit MAX-OPTIMIZE-ITERATIONS limit.")

;;; IR1-Optimize-Until-Done  --  Internal
;;;
;;; Repeatedly optimize Component until no further optimizations can be
;;; found or we hit our iteration limit.  When we hit the limit, we clear
;;; the component and block REOPTIMIZE flags to discourage the following
;;; optimization attempt from pounding on the same code.
;;;
(defun ir1-optimize-until-done (component)
  (declare (type component component))
  (maybe-mumble "Opt")
  (event ir1-optimize-until-done)
  (let ((count 0)
	(cleared-reanalyze nil))
    (loop
      (when (component-reanalyze component)
	(setf count 0)
	(setf cleared-reanalyze t)
	(setf (component-reanalyze component) nil))
      (setf (component-reoptimize component) nil)
      (ir1-optimize component)
      (cond ((component-reoptimize component)
	     (incf count)
	     (when (= count max-optimize-iterations)
	       (maybe-mumble "*")
	       (cond ((retry-delayed-transforms :optimize)
		      (maybe-mumble "+")
		      (setf count 0))
		     (t
		      (event ir1-optimize-maxed-out)
		      (setf (component-reoptimize component) nil)
		      (do-blocks (block component)
			(setf (block-reoptimize block) nil))
		      (return)))))
	    ((retry-delayed-transforms :optimize)
	     (setf count 0)
	     (maybe-mumble "+"))
	    (t
	     (return)))
      (maybe-mumble "."))
    (when cleared-reanalyze
      (setf (component-reanalyze component) t))
    (maybe-mumble " "))
  (undefined-value))

(defparameter *constraint-propagate* t)
(defparameter *reoptimize-after-type-check-max* 10)

(defevent reoptimize-maxed-out
  "*REOPTIMIZE-AFTER-TYPE-CHECK-MAX* exceeded.")

;;; DFO-AS-NEEDED  --  Internal
;;;
;;; Iterate doing FIND-DFO until no new dead code is discovered.
;;;
(defun dfo-as-needed (component)
  (declare (type component component))
  (when (component-reanalyze component)
    (maybe-mumble "DFO")
    (loop
      (find-dfo component)
      (unless (component-reanalyze component)
	(maybe-mumble " ")
	(return))
      (maybe-mumble ".")))
  (undefined-value))

;;; IR1-Phases  --  Internal
;;;
;;; Do all the IR1 phases for a non-top-level component.
;;;
(defun ir1-phases (component)
  (declare (type component component))
  (let ((*constraint-number* 0)
	(loop-count 1)
	(*delayed-transforms* nil))
    (declare (special *constraint-number* *delayed-transforms*))
    (loop
      (ir1-optimize-until-done component)
      (when (or (component-new-functions component)
		(component-reanalyze-functions component))
	(maybe-mumble "Locall ")
	(local-call-analyze component))
      (dfo-as-needed component)
      (when *constraint-propagate*
	(maybe-mumble "Constraint ")
	(constraint-propagate component))
      (when (retry-delayed-transforms :constraint)
	(maybe-mumble "Rtran "))
      ;; Delay the generation of type checks until the type constraints have
      ;; had time to propagate, else the compiler can confuse itself.
      (unless (and (or (component-reoptimize component)
		       (component-reanalyze component)
		       (component-new-functions component)
		       (component-reanalyze-functions component))
		   (< loop-count (- *reoptimize-after-type-check-max* 4)))
	(maybe-mumble "Type ")
	(generate-type-checks component)
	(unless (or (component-reoptimize component)
		    (component-reanalyze component)
		    (component-new-functions component)
		    (component-reanalyze-functions component))
	  (return)))
      (when (>= loop-count *reoptimize-after-type-check-max*)
	(maybe-mumble "[Reoptimize Limit]")
	(event reoptimize-maxed-out)
	(return))
      (incf loop-count)))

  (ir1-finalize component)
  (undefined-value))

;;; Native-Compile-Component  --  Internal
;;;
(defun native-compile-component (component)
  (let ((*code-segment* nil)
	(*elsewhere* nil)
	(*elsewhere-label* nil))
    (maybe-mumble "GTN ")
    (gtn-analyze component)
    (maybe-mumble "LTN ")
    (ltn-analyze component)
    (dfo-as-needed component)
    (maybe-mumble "Control ")
    (control-analyze component #'make-ir2-block)

    (when (ir2-component-values-receivers (component-info component))
      (maybe-mumble "Stack ")
      (stack-analyze component)
      ;;
      ;; Assign BLOCK-NUMBER for any cleanup blocks introduced by stack
      ;; analysis.  There shouldn't be any unreachable code after control, so
      ;; this won't delete anything.
      (dfo-as-needed component))

    (unwind-protect
	(progn
	  (maybe-mumble "IR2Tran ")
	  (init-assembler)
	  (entry-analyze component)  ; FIX mumble "Entry"?
	  (ir2-convert component)

	  (when (policy nil (>= speed cspeed))
	    (maybe-mumble "Copy ")
	    (copy-propagate component))

	  (select-representations component)

	  (when *check-consistency*
	    (maybe-mumble "Check2 ")
	    (check-ir2-consistency component))

	  (delete-unreferenced-tns component)

	  (maybe-mumble "Life ")
	  (lifetime-analyze component)

	  (when *compile-progress*
	    (compiler-mumble "") ; Sync before doing random output.
	    (pre-pack-tn-stats component *compiler-error-output*))

	  (when *check-consistency*
	    (maybe-mumble "CheckL ")
	    (check-life-consistency component))

	  (maybe-mumble "Pack ")
	  (pack component)

	  (when *check-consistency*
	    (maybe-mumble "CheckP ")
	    (check-pack-consistency component))

	  (when *compiler-trace-output*
	    (describe-component component *compiler-trace-output*)
	    (describe-ir2-component component *compiler-trace-output*))

	  (maybe-mumble "Code ")
	  (multiple-value-bind
	      (length trace-table fixups)
	      (generate-code component)

	    (when (and *compiler-trace-output*
		       (backend-disassem-params *backend*))
	      (format *compiler-trace-output*
		      "~|~%Disassembly of code for ~S~2%" component)
	      (disassem:disassemble-assem-segment *code-segment*
						  *compiler-trace-output*
						  *backend*))

	    (etypecase *compile-object*
	      (fasl-file
	       (maybe-mumble "FASL")
	       (fasl-dump-component component *code-segment*
				    length trace-table fixups
				    *compile-object*))
	      (core-object
	       (maybe-mumble "Core")
	       (make-core-component component *code-segment*
				    length trace-table fixups
				    *compile-object*))
	      (null))))

      (when *code-segment*
	(new-assem:release-segment *code-segment*))
      (when *elsewhere*
	(new-assem:release-segment *elsewhere*))))

  ;; We are done, so don't bother keeping anything around.
  (nuke-ir2-component component)
  (setf (component-info component) nil)

  (undefined-value))

;;; BYTE-COMPILING  --  Interface
;;;
;;; Return our best guess for whether we will byte compile code currently
;;; being IR1 converted.  Only a guess because the decision is made on a
;;; per-component basis.
;;;
(defun byte-compiling ()
  (if (eq *byte-compiling* :maybe)
      (or (eq *byte-compile* t)
	  (policy nil (zerop speed) (<= debug 1)))
      (and *byte-compile* *byte-compiling*)))

;;; DELETE-IF-NO-ENTRIES  --  Internal
;;;
;;; Delete components with no external entry points before we try to
;;; generate code.  Unreachable closures can cause IR2 conversion to puke
;;; on itself, since it is the reference to the closure which normally
;;; causes the components to be combined.  This doesn't really cover all
;;; cases...
;;;
(defun delete-if-no-entries (component)
  (dolist (fun (component-lambdas component)
	       (delete-component component))
    (case (functional-kind fun)
      (:top-level (return))
      (:external
       (or (every #'(lambda (ref)
		      (eq (block-component (node-block ref))
			  component))
		  (leaf-refs fun))
	   (return))))))

;;; COMPILE-COMPONENT -- internal.
;;;
(defun compile-component (component)
  (let* ((*compile-component* component)
	 (*byte-compiling*
	  (ecase *byte-compile*
	    ((t) t)
	    ((nil) nil)
	    (:maybe
	     (dolist (fun (component-lambdas component) t)
	       (or (policy (lambda-bind fun)
			   (zerop speed) (<= debug 1))
		   (return nil)))))))

    (if *compile-print*
	(compiler-mumble "~&~:[~;Byte ~]Compiling ~A: "
			 *byte-compiling*
			 (component-name component)))

    (ir1-phases component)

    #|
    (maybe-mumble "Dom ")
    (find-dominators component)
    (maybe-mumble "Loop ")
    (loop-analyze component)
    |#

    (maybe-mumble "Env ")
    (environment-analyze component)
    (dfo-as-needed component)

    (delete-if-no-entries component)

    (or (eq (block-next (component-head component))
	    (component-tail component))
	(if *byte-compiling*
	    (byte-compile-component component)
	    (native-compile-component component))))

  (clear-constant-info)

  (if *compile-print*
      (compiler-mumble "~&"))

  (undefined-value))


;;;; Clearing global data structures.

;;; CLEAR-CONSTANT-INFO  --  Internal
;;;
;;; Clear the INFO in constants in the *FREE-VARIABLES*, etc.  In addition
;;; to allowing stuff to be reclaimed, this is required for correct
;;; assignment of constant offsets, since we need to assign a new offset
;;; for each component.  We don't clear the FUNCTIONAL-INFO slots, since
;;; they are used to keep track of functions across component boundaries.
;;;
(defun clear-constant-info ()
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (setf (leaf-info v) nil))
	   *constants*)

  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (if (constant-p v)
		   (setf (leaf-info v) nil)))
	   *free-variables*)

  (undefined-value))

;;; CLEAR-IR1-INFO  --  Internal
;;;
;;; Blow away the REFS for all global variables, and recycle the IR1 for
;;; Component.
;;;
(defun clear-ir1-info (component)
  (declare (type component component))
  (labels ((blast (x)
	     (maphash #'(lambda (k v)
			  (declare (ignore k))
			  (when (leaf-p v)
			    (setf (leaf-refs v)
				  (delete-if #'here-p (leaf-refs v)))
			    (if (basic-var-p v)
				(setf (basic-var-sets v)
				      (delete-if #'here-p (basic-var-sets v))))))
		      x))
	   (here-p (x)
	     (eq (block-component (node-block x)) component)))
    (blast *free-variables*)
    (blast *free-functions*)
    (blast *constants*))
  (macerate-ir1-component component)
  (undefined-value))

;;; CLEAR-STUFF  --  Interface
;;;
;;; Clear all the global variables used by the compiler.
;;;
(defun clear-stuff (&optional (debug-too t))
  ;;
  ;; Clear global tables.
  (when (boundp '*free-functions*)
    (clrhash *free-functions*)
    (clrhash *free-variables*)
    (clrhash *constants*))

  (when debug-too
    (clrhash *continuation-numbers*)
    (clrhash *number-continuations*)
    (setq *continuation-number* 0)
    (clrhash *tn-ids*)
    (clrhash *id-tns*)
    (setq *tn-id* 0)
    (clrhash *label-ids*)
    (clrhash *id-labels*)
    (setq *label-id* 0)
    ;;
    ;; Clear some Pack data structures (for GC purposes only.)
    (assert (not *in-pack*))
    (dolist (sb (backend-sb-list *backend*))
      (if (finite-sb-p sb)
	  (fill (finite-sb-live-tns sb) nil))))
  ;;
  ;; Reset Gensym.
  (setq lisp:*gensym-counter* 0)

  (values))


;;; PRINT-SUMMARY  --  Interface
;;;
;;; This function is called by WITH-COMPILATION-UNIT at the end of a
;;; compilation unit.  It prints out any residual unknown function warnings
;;; and the total error counts.  Abort-P should be true when the
;;; compilation unit was aborted by throwing out.  Abort-Count is the
;;; number of dynamically enclosed nested compilation units that were
;;; aborted.
;;;
(defun print-summary (abort-p abort-count)
  (unless abort-p
    (handler-bind ((warning #'compiler-warning-handler))
      (let ((undefs (sort *undefined-warnings* #'string<
			  :key #'(lambda (x)
				   (let ((x (undefined-warning-name x)))
				     (if (symbolp x)
					 (symbol-name x)
					 (prin1-to-string x)))))))
	(unless *converting-for-interpreter*
	  (dolist (undef undefs)
	    (let ((name (undefined-warning-name undef))
		  (kind (undefined-warning-kind undef))
		  (warnings (undefined-warning-warnings undef))
		  (count (undefined-warning-count undef)))
	      (dolist (*compiler-error-context* warnings)
		(compiler-warning "Undefined ~(~A~): ~S" kind name))

	      (let ((warn-count (length warnings)))
		(when (and warnings (> count warn-count))
		  (let ((more (- count warn-count)))
		    (compiler-warning "~D more use~:P of undefined ~(~A~) ~S."
				      more kind name)))))))

	(dolist (kind '(:variable :function :type))
	  (let ((summary (mapcar #'undefined-warning-name
				 (remove kind undefs :test-not #'eq
					 :key #'undefined-warning-kind))))
	    (when summary
	      (compiler-warning
	       "~:[This ~(~A~) is~;These ~(~A~)s are~] undefined:~
		~%  ~{~<~%  ~1:;~S~>~^ ~}"
	       (cdr summary) kind summary)))))))

  (unless (or *converting-for-interpreter*
	      (and (not abort-p) (zerop abort-count)
		   (zerop *compiler-error-count*)
		   (zerop *compiler-warning-count*)
		   (zerop *compiler-note-count*)))
    (compiler-mumble
     "~2&Compilation unit ~:[finished~;aborted~].~
      ~[~:;~:*~&  ~D fatal error~:P~]~
      ~[~:;~:*~&  ~D error~:P~]~
      ~[~:;~:*~&  ~D warning~:P~]~
      ~[~:;~:*~&  ~D note~:P~]~2%"
     abort-p
     abort-count
     *compiler-error-count*
     *compiler-warning-count*
     *compiler-note-count*)))


;;;; Trace output.

;;; Describe-Component  --  Internal
;;;
;;; Print out some useful info about Component to Stream.
;;;
(defun describe-component (component *standard-output*)
  (declare (type component component))
  (format t "~|~%;;;; Component: ~S~2%" (component-name component))
  (print-blocks component)
  (undefined-value))

(defun describe-ir2-component (component *standard-output*)
  (format t "~%~|~%;;;; IR2 component: ~S~2%" (component-name component))

  (format t "Entries:~%")
  (dolist (entry (ir2-component-entries (component-info component)))
    (format t "~4TL~D: ~S~:[~; [Closure]~]~%"
	    (label-id (entry-info-offset entry))
	    (entry-info-name entry)
	    (entry-info-closure-p entry)))

  (terpri)
  (pre-pack-tn-stats component *standard-output*)
  (terpri)
  (print-ir2-blocks component)
  (terpri)
  (undefined-value))


;;;; File reading.
;;;
;;; When reading from a file, we have to keep track of some source
;;; information.  We also exploit our ability to back up for printing the
;;; error context and for recovering from errors.
;;;
;;; The interface we provide to this stuff is the stream-oid Source-Info
;;; structure.  The bookkeeping is done as a side-effect of getting the next
;;; source form.

;;; The File-Info structure holds all the source information for a given file.
;;;
(defstruct file-info
  ;;
  ;; If a file, the truename of the corresponding source file.  If from a Lisp
  ;; form, :LISP, if from a stream, :STREAM.
  (name (required-argument) :type (or pathname (member :lisp :stream)))
  ;;
  ;; The defaulted, but not necessarily absolute file name (i.e. prior to
  ;; TRUENAME call.)  Null if not a file.  This is used to set
  ;; *COMPILE-FILE-PATHNAME*, and if absolute, is dumped in the debug-info.
  (untruename nil :type (or pathname null))
  ;;
  ;; The file's write date (if relevant.)
  (write-date nil :type (or unsigned-byte null))
  ;;
  ;; This file's FILE-COMMENT, or NIL if none.
  (comment nil :type (or simple-string null))
  ;;
  ;; The source path root number of the first form in this file (i.e. the
  ;; total number of forms converted previously in this compilation.)
  (source-root 0 :type unsigned-byte)
  ;;
  ;; Parallel vectors containing the forms read out of the file and the file
  ;; positions that reading of each form started at (i.e. the end of the
  ;; previous form.)
  (forms (make-array 10 :fill-pointer 0 :adjustable t) :type (vector t))
  (positions (make-array 10 :fill-pointer 0 :adjustable t) :type (vector t))
  ;;
  ;; Language to use.  Normally Lisp, but sometimes Dylan.
  (language :lisp :type (member :lisp #+nil :dylan)))

;;; The Source-Info structure provides a handle on all the source information
;;; for an entire compilation.
;;;
(defstruct (source-info
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore s d))
	       (format stream "#<Source-Info>"))))
  ;;
  ;; The universal time that compilation started at.
  (start-time (get-universal-time) :type unsigned-byte)
  ;;
  ;; A list of the file-info structures for this compilation.
  (files nil :type list)
  ;;
  ;; The tail of the Files for the file we are currently reading.
  (current-file nil :type list)
  ;;
  ;; The stream that we are using to read the Current-File.  Null if no stream
  ;; has been opened yet.
  (stream nil :type (or stream null)))

;;; Make-File-Source-Info  --  Internal
;;;
;;; Given a list of pathnames, return a Source-Info structure.
;;;
(defun make-file-source-info (files)
  (declare (list files))
  (let ((file-info
	 (mapcar #'(lambda (x)
		     (make-file-info :name (truename x)
				     :untruename x
				     :write-date (file-write-date x)
				     :language :lisp))
		 files)))

    (make-source-info :files file-info
		      :current-file file-info)))

;;; MAKE-LISP-SOURCE-INFO  --  Interface
;;;
;;; Return a SOURCE-INFO to describe the incremental compilation of Form.
;;; Also used by EVAL:INTERNAL-EVAL.
;;;
(defun make-lisp-source-info (form)
  (make-source-info
   :start-time (get-universal-time)
   :files (list (make-file-info :name :lisp
				:forms (vector form)
				:positions '#(0)))))

;;; MAKE-STREAM-SOURCE-INFO  --  Internal
;;;
;;; Return a SOURCE-INFO which will read from Stream.
;;;
(defun make-stream-source-info (stream language)
  (declare (type (member :lisp #+nil :dylan) language))
  (let ((files (list (make-file-info :name :stream :language language))))
    (make-source-info
     :files files
     :current-file files
     :stream stream)))

;;; Normal-Read-Error  --  Internal
;;;
;;; Print an error message for a non-EOF error on Stream.  Old-Pos is a
;;; preceding file position that hopefully comes before the beginning of
;;; the line.  Of course, this only works on streams that support the
;;; file-position operation.
;;;
(defun normal-read-error (stream old-pos condition)
  (declare (type stream stream) (type unsigned-byte old-pos))
  (let ((pos (file-position stream)))
    (file-position stream old-pos)
    (let ((start old-pos))
      (loop
	(let ((line (read-line stream nil))
	      (end (file-position stream)))
	  (when (>= end pos)
	    (compiler-error-message
	     "Read error at ~D:~% \"~A/\\~A\"~%~A"
	     pos
	     (string-left-trim " 	"
			       (subseq line 0 (- pos start)))
	     (subseq line (- pos start))
	     condition)
	    (return))
	  (setq start end)))))
  (undefined-value))

;;; Ignore-Error-Form  --  Internal
;;;
;;; Back Stream up to the position Pos, then read a form with
;;; *Read-Suppress* on, discarding the result.  If an error happens during
;;; this read, then bail out using Compiler-Error (fatal in this context).
;;;
(defun ignore-error-form (stream pos)
  (declare (type stream stream) (type unsigned-byte pos))
  (file-position stream pos)
  (handler-case (let ((*read-suppress* t)
		      (*features* (backend-features *target-backend*)))
		  (read stream))
    (error (condition)
      (declare (ignore condition))
      (compiler-error "Unable to recover from read error."))))

;;; Unexpected-EOF-Error  --  Internal
;;;
;;; Print an error message giving some context for an EOF error.  We print
;;; the first line after Pos that contains #\" or #\(, or lacking that, the
;;; first non-empty line.
;;;
(defun unexpected-eof-error (stream pos condition)
  (declare (type stream stream) (type unsigned-byte pos))
  (let ((res nil))
    (file-position stream pos)
    (loop
      (let ((line (read-line stream nil nil)))
	(unless line (return))
	(when (or (find #\" line) (find #\( line))
	  (setq res line)
	  (return))
	(unless (or res (zerop (length line)))
	  (setq res line))))

    (compiler-error-message
     "Read error in form starting at ~D:~%~@[ \"~A\"~%~]~A"
     pos res condition))

  (file-position stream (file-length stream))
  (undefined-value))

;;; Careful-Read  --  Internal
;;;
;;; Read a form from Stream, returning EOF at EOF.  If a read error
;;; happens, then attempt to recover if possible, returning a proxy error
;;; form.
;;;
(defun careful-read (stream eof pos)
  (handler-case (let ((*features* (backend-features *target-backend*)))
		  (read stream nil eof))
    (error (condition)
      (let ((new-pos (file-position stream)))
	(cond ((= new-pos (file-length stream))
	       (unexpected-eof-error stream pos condition))
	      (t
	       (normal-read-error stream pos condition)
	       (ignore-error-form stream pos))))
      '(cerror "Skip this form."
	       "Attempt to load a file having a compile-time read error."))))

;;; Get-Source-Stream  --  Internal
;;;
;;; If Stream is present, return it, otherwise open a stream to the current
;;; file.  There must be a current file.  When we open a new file, we also
;;; reset *Package* and policy.  This gives the effect of rebinding around
;;; each file.
;;;
(defun get-source-stream (info)
  (declare (type source-info info))
  (cond ((source-info-stream info))
	(t
	 (setq *package* *initial-package*)
	 (setq *default-cookie* (copy-cookie *initial-cookie*))
	 (setq *default-interface-cookie*
	       (copy-cookie *initial-interface-cookie*))
	 (let* ((finfo (first (source-info-current-file info)))
		(name (file-info-name finfo)))
	   (setq *compile-file-truename* name)
	   (setq *compile-file-pathname* (file-info-untruename finfo))
	   (setf (source-info-stream info)
		 (open name :direction :input))))))

;;; CLOSE-SOURCE-INFO  --  Internal
;;;
;;; Close the stream in Info if it is open.
;;;
(defun close-source-info (info)
  (declare (type source-info info))
  (let ((stream (source-info-stream info)))
    (when stream (close stream)))
  (setf (source-info-stream info) nil)
  (undefined-value))

;;; Advance-Source-File  --  Internal
;;;
;;; Advance Info to the next source file.  If none, return NIL, otherwise T.
;;;
(defun advance-source-file (info)
  (declare (type source-info info))
  (close-source-info info)
  (let ((prev (pop (source-info-current-file info))))
    (if (source-info-current-file info)
	(let ((current (first (source-info-current-file info))))
	  (setf (file-info-source-root current)
		(+ (file-info-source-root prev)
		   (length (file-info-forms prev))))
	  t)
	nil)))

;;; PROCESS-SOURCES -- internal.
;;;
;;; Read the sources from the source files and process them.
;;;
(defun process-sources (info)
  (let* ((file (first (source-info-current-file info)))
	 (language (file-info-language file))
	 (stream (get-source-stream info)))
    (ecase language
      (:lisp
       (loop
	 (let* ((pos (file-position stream))
		(eof '(*eof*))
		(form (careful-read stream eof pos)))
	   (if (eq form eof)
	       (return)
	       (let* ((forms (file-info-forms file))
		      (current-idx (+ (fill-pointer forms)
				      (file-info-source-root file))))
		 (vector-push-extend form forms)
		 (vector-push-extend pos (file-info-positions file))
		 (clrhash *source-paths*)
		 (find-source-paths form current-idx)
		 (process-form form
			       `(original-source-start 0 ,current-idx))))))))
    (when (advance-source-file info)
      (process-sources info))))

;;; FIND-FILE-INFO  --  Interface
;;;
;;; Return the File-Info describing the Index'th form.
;;;
(defun find-file-info (index info)
  (declare (type index index) (type source-info info))
  (dolist (file (source-info-files info))
    (when (> (+ (length (file-info-forms file))
		(file-info-source-root file))
	     index)
      (return file))))

;;; FIND-SOURCE-ROOT  --  Interface
;;;
;;; Return the Index'th source form read from Info and the position that it
;;; was read at.
;;;
(defun find-source-root (index info)
  (declare (type source-info info) (type index index))
  (let* ((file (find-file-info index info))
	 (idx (- index (file-info-source-root file))))
    (values (aref (file-info-forms file) idx)
	    (aref (file-info-positions file) idx))))


;;;; Top-level form processing.

;;; CONVERT-AND-MAYBE-COMPILE  --  Internal
;;;
;;; Called by top-level form processing when we are ready to actually
;;; compile something.  If *BLOCK-COMPILE* is T, then we still convert the
;;; form, but delay compilation, pushing the result on *TOP-LEVEL-LAMBDAS*
;;; instead.
;;;
;;; The cookies at this time becomes the default policy for compiling the
;;; form.  Any enclosed PROCLAIMs will affect only subsequent forms.
;;;
(defun convert-and-maybe-compile (form path)
  (declare (list path))
  (let ((orig (bytes-consed-between-gcs)))
    (unwind-protect
	(progn
	  (setf (bytes-consed-between-gcs) (* orig 4))
	  (let* ((*lexical-environment*
		  (make-lexenv :cookie *default-cookie*
			       :interface-cookie *default-interface-cookie*))
		 (tll (ir1-top-level form path nil)))
	    (cond ((eq *block-compile* t) (push tll *top-level-lambdas*))
		  (t
		   (compile-top-level (list tll) nil)))))
      (setf (bytes-consed-between-gcs) orig))))

;;; PROCESS-PROGN  --  Internal
;;;
;;; Process a PROGN-like portion of a top-level form.  Forms is a list of
;;; the forms, and Path is source path of the form they came out of.
;;;
(defun process-progn (forms path)
  (declare (list forms) (list path))
  (dolist (form forms)
    (process-form form path)))

;;; PREPROCESSOR-MACROEXPAND  --  Internal
;;;
;;; Macroexpand form in the current environment with an error handler.  We
;;; only expand one level, so that we retain all the intervening forms in
;;; the source path.
;;;
(defun preprocessor-macroexpand (form)
  (handler-case (macroexpand-1 form *lexical-environment*)
    (error (condition)
       (compiler-error "(during macroexpansion)~%~A" condition))))

;;; PROCESS-LOCALLY  --  Internal
;;;
;;; Process a top-level use of LOCALLY.  We parse declarations and then
;;; recursively process the body.
;;;
;;; Binding *DEFAULT-xxx-COOKIE* is pretty much of a hack, since it causes
;;; LOCALLY to "capture" enclosed proclamations.  It is necessary because
;;; CONVERT-AND-MAYBE-COMPILE uses the value of *DEFAULT-COOKIE* as the
;;; policy.  The need for this hack is due to the quirk that there is no
;;; way to represent in a cookie that an optimize quality came from the
;;; default.
;;;
(defun process-locally (form path)
  (declare (list path))
  (multiple-value-bind
      (body decls)
      (system:parse-body (cdr form) *lexical-environment* nil)
    (let* ((*lexical-environment*
	    (process-declarations decls nil nil (make-continuation)))
	   (*default-cookie* (lexenv-cookie *lexical-environment*))
	   (*default-interface-cookie*
	    (lexenv-interface-cookie *lexical-environment*)))
      (process-progn body path))))

;;; PROCESS-FILE-COMMENT  --  Internal
;;;
;;; Stash file comment in the file-info structure.
;;;
(defun process-file-comment (form)
  (or (and (= (length form) 2) (stringp (second form)))
      (compiler-error "Bad FILE-COMMENT form: ~S." form))
  (let ((file (first (source-info-current-file *source-info*))))
    (cond ((file-info-comment file)
	   (compiler-warning "Ignoring extra file comment:~%  ~S." form))
	  (t
	   (let ((comment (coerce (second form) 'simple-string)))
	     (setf (file-info-comment file) comment)
	     (when *compile-verbose*
	       (compiler-mumble "~&Comment: ~A~2&" comment)))))))

;;; PROCESS-COLD-LOAD-FORM  --  Internal
;;;
;;; Force any pending top-level forms to be compiled and dumped so that
;;; they will be evaluated in the correct package environment.  Eval the
;;; form if Eval is true, then dump the form to evaled at (cold) load time.
;;;
(defun process-cold-load-form (form path eval)
  (let ((object *compile-object*))
    (etypecase object
      (fasl-file
       (compile-top-level-lambdas () t)
       (fasl-dump-cold-load-form form object))
      ((or null core-object)
       (convert-and-maybe-compile form path)))
    (when eval (eval form))))

;;; PROCESS-PROCLAIM  --  Internal
;;;
;;; If a special block compilation delimiter, then start or end the block
;;; as appropriate.  Otherwise, just convert-and-maybe-compile the form.
;;; If :block-compile was T or NIL, then we ignore any start/end block
;;; declarations.
;;;
(defun process-proclaim (form path)
  (if (and (eql (length form) 2) (constantp (cadr form)))
      (let ((spec (eval (cadr form))))
	(if (and (consp spec) (eq *block-compile-argument* :specified))
	    (case (first spec)
	      (start-block
	       (finish-block-compilation)
	       (setq *block-compile* t)
	       (setq *entry-points* (rest spec)))
	      (end-block
	       (finish-block-compilation))
	      (t
	       (convert-and-maybe-compile form path)))
	    (convert-and-maybe-compile form path)))
      (convert-and-maybe-compile form path)))

(proclaim '(special *compiler-error-bailout*))

;;; PROCESS-FORM  --  Internal
;;;
;;; Process a top-level Form with the specified source Path and FIX? output
;;; to Object.
;;;
;;; -- If this is a magic top-level form, then do stuff.
;;; -- If it is a macro expand it.
;;; -- Otherwise, just compile it.
;;;
(defun process-form (form path)
  (declare (list path))
  (catch 'process-form-error-abort
    (let* ((path (or (gethash form *source-paths*) (cons form path)))
	   (*compiler-error-bailout*
	    #'(lambda ()
		(convert-and-maybe-compile
		 `(error "Execution of a form compiled with errors:~% ~S"
			 ',form)
		 path)
		(throw 'process-form-error-abort nil))))
      (if (atom form)
	  (convert-and-maybe-compile form path)
	  (case (car form)
	    ((make-package shadow shadowing-import export
	      unexport use-package unuse-package import
	      old-in-package %in-package %defpackage)
	     (process-cold-load-form form path t))
	    ((error cerror break signal)
	     (process-cold-load-form form path nil))
	    (kernel:%compiler-defstruct
	     (convert-and-maybe-compile form path)
	     (compile-top-level-lambdas () t))
	    ((eval-when)
	     (or (>= (length form) 2)
		 (compiler-error "EVAL-WHEN form is too short: ~S." form))
	     (do-eval-when-stuff
	      (cadr form) (cddr form)
	      #'(lambda (forms)
		  (process-progn forms path))))
	    ((macrolet)
	     (or (>= (length form) 2)
		 (compiler-error "MACROLET form is too short: ~S." form))
	     (do-macrolet-stuff
	      (cadr form)
	      #'(lambda ()
		  (process-progn (cddr form) path))))
	    (locally (process-locally form path))
	    (progn (process-progn (cdr form) path))
	    (file-comment (process-file-comment form))
	    (proclaim (process-proclaim form path))
	    (t
	     (let ((exp (preprocessor-macroexpand form)))
	       (if (eq exp form)
		   (convert-and-maybe-compile form path)
		   (process-form exp path))))))))

  (undefined-value))


;;;; Load time value support.

;;; See EMIT-MAKE-LOAD-FORM.

;;; PRODUCING-FASL-FILE  --  interface.
;;;
;;; Returns T iff we are currently producing a fasl-file and hence constants
;;; need to be dumped carfully.
;;;
(defun producing-fasl-file ()
  (unless *converting-for-interpreter*
    (fasl-file-p *compile-object*)))

;;; COMPILE-LOAD-TIME-VALUE  --  interface.
;;;
;;; Compile FORM and arrange for it to be called at load-time.  Returns the
;;; dumper handle and our best guess at the type of the object.
;;;
(defun compile-load-time-value
       (form &optional
	     (name (let ((*print-level* 2) (*print-length* 3))
		     (format nil "Load Time Value of ~S"
			     (if (and (listp form)
				      (eq (car form) 'make-value-cell))
				 (second form)
				 form)))))
  (let ((lambda (compile-load-time-stuff form name t)))
    (values
     (fasl-dump-load-time-value-lambda lambda *compile-object*)
     (let ((type (leaf-type lambda)))
       (if (function-type-p type)
	   (single-value-type (function-type-returns type))
	   *wild-type*)))))

;;; COMPILE-MAKE-LOAD-FORM-INIT-FORMS  --  internal.
;;;
;;; Compile the FORMS and arrange for them to be called (for effect, not value)
;;; at load-time.
;;;
(defun compile-make-load-form-init-forms (forms name)
  (let ((lambda (compile-load-time-stuff `(progn ,@forms) name nil)))
    (fasl-dump-top-level-lambda-call lambda *compile-object*)))

;;; COMPILE-LOAD-TIME-STUFF  --  internal.
;;;
;;; Does the actual work of COMPILE-LOAD-TIME-VALUE or COMPILE-MAKE-LOAD-FORM-
;;; INIT-FORMS.
;;;
(defun compile-load-time-stuff (form name for-value)
  (with-ir1-namespace
   (let* ((*lexical-environment* (make-null-environment))
	  (lambda (ir1-top-level form *current-path* for-value)))
     (setf (leaf-name lambda) name)
     (compile-top-level (list lambda) t)
     lambda)))

;;; COMPILE-LOAD-TIME-VALUE-LAMBDA  --  internal.
;;;
;;; Called by COMPILE-TOP-LEVEL when it was pased T for LOAD-TIME-VALUE-P
;;; (which happens in COMPILE-LOAD-TIME-STUFF).  We don't try to combine
;;; this component with anything else and frob the name.  If not in a
;;; :TOP-LEVEL component, then don't bother compiling, because it was merged
;;; with a run-time component.
;;;
(defun compile-load-time-value-lambda (lambdas)
  (assert (null (cdr lambdas)))
  (let* ((lambda (car lambdas))
	 (component (block-component (node-block (lambda-bind lambda)))))
    (when (eq (component-kind component) :top-level)
      (setf (component-name component) (leaf-name lambda))
      (compile-component component)
      (clear-ir1-info component))))

;;; EMIT-MAKE-LOAD-FORM  --  interface.
;;;
;;; The entry point for MAKE-LOAD-FORM support.  When IR1 conversion finds a
;;; constant structure, it invokes this to arrange for proper dumping.  If it
;;; turns out that the constant has already been dumped, then we don't need
;;; to do anything.
;;;
;;; If the constant hasn't been dumped, then we check to see if we are in the
;;; process of creating it.  We detect this by maintaining the special
;;; *constants-being-created* as a list of all the constants we are in the
;;; process of creating.  Actually, each entry is a list of the constant and
;;; any init forms that need to be processed on behalf of that constant.
;;;
;;; It's not necessarily an error for this to happen.  If we are processing the
;;; init form for some object that showed up *after* the original reference
;;; to this constant, then we just need to defer the processing of that init
;;; form.  To detect this, we maintain *constants-created-since-last-init* as
;;; a list of the constants created since the last time we started processing
;;; an init form.  If the constant passed to emit-make-load-form shows up in
;;; this list, then there is a circular chain through creation forms, which is
;;; an error.
;;;
;;; If there is some intervening init form, then we blow out of processing it
;;; by throwing to the tag PENDING-INIT.  The value we throw is the entry from
;;; *constants-being-created*.  This is so the offending init form can be
;;; tacked onto the init forms for the circular object.
;;;
;;; If the constant doesn't show up in *constants-being-created*, then we have
;;; to create it.  We call MAKE-LOAD-FORM and check to see if the creation
;;; form is the magic value :just-dump-it-normally.  If it is, then we don't
;;; do anything.  The dumper will eventually get it's hands on the object
;;; and use the normal structure dumping noise on it.
;;;
;;; Otherwise, we bind *constants-being-created* and *constants-created-since-
;;; last-init* and compile the creation form a la load-time-value.  When this
;;; finishes, we tell the dumper to use that result instead whenever it sees
;;; this constant.
;;;
;;; Now we try to compile the init form.  We bind *constants-created-since-
;;; last-init* to NIL and compile the init form (and any init forms that were
;;; added because of circularity detection).  If this works, great.  If not,
;;; we add the init forms to the init forms for the object that caused the
;;; problems and let it deal with it.
;;;
(defvar *constants-being-created* nil)
(defvar *constants-created-since-last-init* nil)
;;;
(defun emit-make-load-form (constant)
  (assert (fasl-file-p *compile-object*))
  (unless (fasl-constant-already-dumped constant *compile-object*)
    (let ((circular-ref (assoc constant *constants-being-created* :test #'eq)))
      (when circular-ref
	(when (find constant *constants-created-since-last-init* :test #'eq)
	  (throw constant t))
	(throw 'pending-init circular-ref)))
    (multiple-value-bind
	(creation-form init-form)
	(handler-case
	    (if (fboundp 'lisp::make-load-form)
		(locally
		 (declare (optimize (inhibit-warnings 3)))
		 (lisp::make-load-form constant (make-null-environment)))
		(make-structure-load-form constant))
	  (error (condition)
		 (compiler-error "(while making load form for ~S)~%~A"
				 constant condition)))
      (case creation-form
	(:just-dump-it-normally
	 (fasl-validate-structure constant *compile-object*)
	 t)
	(:ignore-it
	 nil)
	(t
	 (compile-top-level-lambdas () t)
	 (when (fasl-constant-already-dumped constant *compile-object*)
	   (return-from emit-make-load-form nil))
	 (let* ((name (let ((*print-level* 1) (*print-length* 2))
			(with-output-to-string (stream)
			  (write constant :stream stream))))
		(info (if init-form
			  (list constant name init-form)
			  (list constant))))
	   (let ((*constants-being-created*
		  (cons info *constants-being-created*))
		 (*constants-created-since-last-init*
		  (cons constant *constants-created-since-last-init*)))
	     (when
		 (catch constant
		   (fasl-note-handle-for-constant
		    constant
		    (compile-load-time-value
		     creation-form
		     (format nil "Creation Form for ~A" name))
		    *compile-object*)
		   nil)
	       (compiler-error "Circular references in creation form for ~S"
			       constant)))
	   (when (cdr info)
	     (let* ((*constants-created-since-last-init* nil)
		    (circular-ref
		     (catch 'pending-init
		       (loop for (name form) on (cdr info) by #'cddr
			 collect name into names
			 collect form into forms
			 finally
			 (compile-make-load-form-init-forms
			  forms
			  (format nil "Init Form~:[~;s~] for ~{~A~^, ~}"
				  (cdr forms) names)))
		       nil)))
	       (when circular-ref
		 (setf (cdr circular-ref)
		       (append (cdr circular-ref) (cdr info))))))))))))


;;;; COMPILE-FILE and COMPILE-FROM-STREAM.

;;; We build a list of top-level lambdas, and then periodically smash them
;;; together into a single component and compile it.
;;;
(defvar *pending-top-level-lambdas*)

;;; The maximum number of top-level lambdas we put in a single top-level
;;; component.
;;;
(defparameter top-level-lambda-max 10)

;;; OBJECT-CALL-TOP-LEVEL-LAMBDA  --  Internal
;;;
(defun object-call-top-level-lambda (tll)
  (declare (type functional tll))
  (let ((object *compile-object*))
    (etypecase object
      (fasl-file
       (fasl-dump-top-level-lambda-call tll object))
      (core-object
       (core-call-top-level-lambda tll object))
      (null))))

;;; SUB-COMPILE-TOP-LEVEL-LAMBDAS  --  Internal
;;;
;;; Add Lambdas to the pending lambdas.  If this leaves more than
;;; TOP-LEVEL-LAMBDA-MAX lambdas in the list, or if Force-P is true, then
;;; smash the lambdas into a single component, compile it, and call the
;;; resulting function.
;;;
(defun sub-compile-top-level-lambdas (lambdas force-p)
  (declare (list lambdas))
  (setq *pending-top-level-lambdas*
	(append *pending-top-level-lambdas* lambdas))
  (let ((pending *pending-top-level-lambdas*))
    (when (and pending
	       (or (> (length pending) top-level-lambda-max)
		   force-p))
      (multiple-value-bind (component tll)
			   (merge-top-level-lambdas pending)
	(setq *pending-top-level-lambdas* ())
	(let ((*byte-compile* (if (eq *byte-compile* :maybe)
				  *byte-compile-top-level*
				  *byte-compile*)))
	  (compile-component component))
	(clear-ir1-info component)
	(object-call-top-level-lambda tll))))
  (undefined-value))

;;; COMPILE-TOP-LEVEL-LAMBDAS  --  Internal
;;;
;;; Compile top-level code and call the Top-Level lambdas.  We pick off
;;; top-level lambdas in non-top-level components here, calling SUB-c-t-l-l
;;; on each subsequence of normal top-level lambdas.
;;;
(defun compile-top-level-lambdas (lambdas force-p)
  (declare (list lambdas))
  (let ((len (length lambdas)))
    (flet ((loser (start)
	     (or (position-if #'(lambda (x)
				  (not (eq (component-kind
					    (block-component
					     (node-block
					      (lambda-bind x))))
					   :top-level)))
			      lambdas
			      :start start)
		 len)))
      (do* ((start 0 (1+ loser))
	    (loser (loser start) (loser start)))
	   ((>= start len)
	    (when force-p
	      (sub-compile-top-level-lambdas nil t)))
	(sub-compile-top-level-lambdas (subseq lambdas start loser)
				       (or force-p (/= loser len)))
	(unless (= loser len)
	  (object-call-top-level-lambda (elt lambdas loser))))))
  (undefined-value))

;;; Compile-Top-Level  --  Internal
;;;
;;; Compile Lambdas (a list of the lambdas for top-level forms) into the
;;; Object file.  We loop doing local call analysis until it converges,
;;; since a single pass might miss something due to components being joined
;;; by let conversion.
;;;
(defun compile-top-level (lambdas load-time-value-p)
  (declare (list lambdas))
  (maybe-mumble "Locall ")
  (loop
    (let ((did-something nil))
      (dolist (lambda lambdas)
	(let* ((component (block-component (node-block (lambda-bind lambda))))
	       (*all-components* (list component)))
	  (when (component-new-functions component)
	    (setq did-something t)
	    (local-call-analyze component))))
      (or did-something (return))))

  (maybe-mumble "IDFO ")
  (multiple-value-bind (components top-components hairy-top)
		       (find-initial-dfo lambdas)
    (let ((*all-components* (append components top-components))
	  (top-level-closure nil))
      (when *check-consistency*
	(maybe-mumble "[Check]~%")
	(check-ir1-consistency *all-components*))

      (dolist (component (append hairy-top top-components))
	(when (pre-environment-analyze-top-level component)
	  (setq top-level-closure t)))

      (let ((*byte-compile*
	     (if (and top-level-closure (eq *byte-compile* :maybe))
		 nil
		 *byte-compile*)))
	(dolist (component components)
	  (compile-component component)
	  (when (replace-top-level-xeps component)
	    (setq top-level-closure t)))

	(when *check-consistency*
	  (maybe-mumble "[Check]~%")
	  (check-ir1-consistency *all-components*))

	(if load-time-value-p
	    (compile-load-time-value-lambda lambdas)
	    (compile-top-level-lambdas lambdas top-level-closure)))

      (dolist (component components)
	(clear-ir1-info component))
      (clear-stuff)))
  (undefined-value))

;;; FINISH-BLOCK-COMPILATION  --  Internal
;;;
;;; Actually compile any stuff that has been queued up for block
;;; compilation.
;;;
(defun finish-block-compilation ()
  (when *block-compile*
    (when *top-level-lambdas*
      (compile-top-level (nreverse *top-level-lambdas*) nil)
      (setq *top-level-lambdas* ()))
    (setq *block-compile* :specified)
    (setq *entry-points* nil)))

;;; Sub-Compile-File  --  Internal
;;;
;;; Read all forms from Info and compile them, FIX? with output to Object.
;;; We return :ERROR, :WARNING, :NOTE or NIL to indicate the most severe
;;; kind of compiler diagnostic emitted.
;;;
(defun sub-compile-file (info &optional d-s-info)
  (declare (type source-info info))
  (with-ir1-namespace
    (let* ((*block-compile* *block-compile-argument*)
	   (start-errors *compiler-error-count*)
	   (start-warnings *compiler-warning-count*)
	   (start-notes *compiler-note-count*)
	   (*package* *package*)
	   (*initial-package* *package*)
	   (*initial-cookie* *default-cookie*)
	   (*initial-interface-cookie* *default-interface-cookie*)
	   (*default-cookie* (copy-cookie *initial-cookie*))
	   (*default-interface-cookie*
	    (copy-cookie *initial-interface-cookie*))
	   (*lexical-environment* (make-null-environment))
	   (*converting-for-interpreter* nil)
	   (*source-info* info)
	   (*compile-file-pathname* nil)
	   (*compile-file-truename* nil)
	   (*top-level-lambdas* ())
	   (*pending-top-level-lambdas* ())
	   (*compiler-error-bailout*
	    #'(lambda ()
		(compiler-mumble
		 "~2&Fatal error, aborting compilation...~%")
		(return-from sub-compile-file :error)))
	   (*current-path* nil)
	   (*last-source-context* nil)
	   (*last-original-source* nil)
	   (*last-source-form* nil)
	   (*last-format-string* nil)
	   (*last-format-args* nil)
	   (*last-message-count* 0)
	   (*info-environment*
	    (or (backend-info-environment *backend*)
		*info-environment*))
	   (*gensym-counter* 0))
      (with-debug-counters
	(clear-stuff)
	(with-compilation-unit ()
	  (process-sources info)

	  (finish-block-compilation)
	  (compile-top-level-lambdas () t)
	  (let ((object *compile-object*))
	    (etypecase object
	      (fasl-file (fasl-dump-source-info info object))
	      (core-object (fix-core-source-info info object d-s-info))
	      (null)))

	  (cond ((> *compiler-error-count* start-errors) :error)
		((> *compiler-warning-count* start-warnings) :warning)
		((> *compiler-note-count* start-notes) :note)
		(t nil)))))))

;;; Verify-Source-Files  --  Internal
;;;
;;; Return a list of pathnames for the named files.  All the files must
;;; exist.
;;;
(defun verify-source-files (stuff)
  (let* ((stuff (if (listp stuff) stuff (list stuff)))
	 (default-host (make-pathname
			:host (pathname-host (pathname (first stuff))))))
    (flet ((try-with-type (path type error-p)
	     (let ((new (merge-pathnames
			 path (make-pathname :type type
					     :defaults default-host))))
	       (if (probe-file new)
		   new
		   (and error-p (truename new))))))
      (unless stuff
	(error "Can't compile with no source files."))
      (mapcar #'(lambda (x)
		  (let ((x (pathname x)))
		    (cond ((logical-pathname-p x)
			   (try-with-type x "LISP" t))
			  ((probe-file x) x)
			  ((try-with-type x "lisp"  nil))
			  ((try-with-type x "lisp"  t)))))
	      stuff))))

;;; COMPILE-FROM-STREAM  --  Public
;;;
;;; Just call SUB-COMPILE-FILE on a stream source info for the stream,
;;; sending output to core.
;;;
(defun compile-from-stream
       (stream &key
	       ((:error-stream *compiler-error-output*) *error-output*)
	       ((:trace-stream *compiler-trace-output*) nil)
	       ((:verbose *compile-verbose*) *compile-verbose*)
	       ((:print *compile-print*) *compile-print*)
	       ((:progress *compile-progress*) *compile-progress*)
	       ((:block-compile *block-compile-argument*)
		*block-compile-default*)
	       ((:entry-points *entry-points*) nil)
	       ((:byte-compile *byte-compile*) *byte-compile-default*)
	       source-info
	       (language :lisp))
  ;; FIX + other optns
  "Read code from $stream until the end of file is reached, compiling it
   into the current environment.  Close $stream when compilation is
   complete.

   Return the first value true only if compiler diagnostics were issued.
   Return the second value true if the only compiler diagnostics issued
   were style warnings.  That is, a true third value indicates that there
   were \"serious\" compiler diagnostics issued, or that a condition of
   type error or warning (other than style-warning) was signalled during
   compilation.

   Options available via keywords:

     $error-stream

	 Write compiler error output to this stream.

     $trace-stream

	 If specified, write several of the internal intermediate
	 compiler representations (including annotated assembly code) to
	 this stream.

     $block-compile {#t, (), :specified}

	 If #t then compile all forms in from the stream as a unit,
	 resolving function references at compile time.

	 If :specified then resolve names at compile time when convenient
	 (as in a self-recursive call), leaving the resolution of top-level
	 DEFUNs for FIX load/run time.  With :specified, anable block
	 compilation on encountering an explicit \"start-block\"
	 declaration.

	 If () then leave the resolution of all global function names for
	 FIX load/run time.

     $source-info

	 Some object to be placed in the DEBUG-SOURCE-INFO. FIX

     $byte-compile {#t, (), :maybe}

         If true, then may compile to interpreted byte code."
  (declare (type (member :lisp #+nil :dylan) language))
  (let ((info (make-stream-source-info stream language))
	(*backend* *native-backend*))
    (unwind-protect
	(let* ((*compile-object* (make-core-object))
	       (won (sub-compile-file info source-info)))
	  (values (not (null won))
		  (if (member won '(:error :warning)) t nil)))
      (close-source-info info))))

(defun elapsed-time-to-string (tsec)
  (multiple-value-bind (tmin sec)
		       (truncate tsec 60)
    (multiple-value-bind (thr min)
			 (truncate tmin 60)
      (format nil "~D:~2,'0D:~2,'0D" thr min sec))))

;;; START-ERROR-OUTPUT, FINISH-ERROR-OUTPUT  --  Internal
;;;
;;; Print some junk at the beginning and end of compilation.
;;;
(defun start-error-output (source-info)
  (declare (type source-info source-info))
  (compiler-mumble "~2&Python version ~A, VM version ~A on ~A.~%"
		   compiler-version (backend-version *backend*)
		   (ext:format-universal-time nil (get-universal-time)
					      :style :government
					      :print-weekday nil
					      :print-timezone nil))
  (dolist (x (source-info-files source-info))
    (compiler-mumble "Compiling: ~A ~A~%"
		     (namestring (file-info-name x))
		     (ext:format-universal-time nil (file-info-write-date x)
						:style :government
						:print-weekday nil
						:print-timezone nil)))
  (compiler-mumble "~%")
  (undefined-value))
;;;
(defun finish-error-output (source-info won)
  (declare (type source-info source-info))
  (compiler-mumble "~&Compilation ~:[aborted after~;finished in~] ~A.~&"
		   won
		   (elapsed-time-to-string
		    (- (get-universal-time)
		       (source-info-start-time source-info))))
  (undefined-value))

;;; COMPILE-FILE  --  Public.
;;;
;;; Open some files and call SUB-COMPILE-FILE.  If something unwinds out of
;;; the compile, then abort the writing of the output file, so that the
;;; original remains intact.
;;;
(defun compile-file (source &key
			    (output-file t)
			    error-file
			    trace-file
			    (error-output t)
			    load
			    ((:verbose *compile-verbose*) *compile-verbose*)
			    ((:print *compile-print*) *compile-print*)
			    ((:progress *compile-progress*) *compile-progress*)
			    ((:block-compile *block-compile-argument*)
			     *block-compile-default*)
			    ((:entry-points *entry-points*) nil)
			    ((:byte-compile *byte-compile*)
			     *byte-compile-default*))
  "Compile $source, producing a corresponding .FASL file.  $source may be a
   list of files, in which case the files are compiled as a unit, producing
   a single .FASL file.

   Return as first value the truename of the output file, or () on failure
   to create the file.  Return the second value true only if compiler
   diagnostics were issued.  Return the third value true if the only
   compiler diagnostics issued were style warnings.  That is, a true third
   value indicates that there were \"serious\" compiler diagnostics issued,
   or that a condition of type error or warning (other than style-warning)
   was signalled during compilation.

   Options available via keywords:

     $output-file

	 The name of the FASL file to output, or () to suppress output, or #t
	 to create the file name from the input file.

     $error-file

	 The name of a file which is deleted before compilation, and to which
	 any error output is directed.  If (), suppress any action on an
	 output file.  If #t, use the name of the first input file name,
	 adding the suffix \".err\".

     $trace-file

	 If specified, dump several of the internal intermediate compiler
	 representations (including annotated assembly code) to this file.
	 If #t create the file name from the first input file.  As in
	 [Trace Files and Disassembly].

     $load

	 If true, load the resulting output file.

     $error-output

	 If #t, then send error output sent to *error-output*.  If a stream,
	 then sent the output to the stream instead.  If (), then suppress
	 error output.

	 This error output is in addition to (and the same as) the output
	 placed in the $error-file.

     $verbose

	 If #t then set *compile-verbose* so that the compiler prints to
	 error output at the start and end of compilation of each file.

     $block-compile {() | :specified | #t}

	 If #t then compile all forms in the file(s) as a unit, resolving
	 function references at compile time.

	 If :specified then resolve names at compile time when convenient
	 (as in a self-recursive call), leaving the resolution of top-level
	 DEFUNs for FIX load/run time.  With :specified, anable block
	 compilation on encountering an explicit \"start-block\"
	 declaration.

	 If () then leave the resolution of all global function names for
	 FIX load/run time.  \xlref{compile-file-block}.

     $entry-points

	 A list of function names for functions in the file(s) that must be
	 given global definitions.  This only applies to block compilation,
	 and is useful mainly when $block-compile #t is specified on a file
	 that lacks \"start-block\" declarations.  If the value is () then
	 all functions will be globally defined.

     $byte-compile {#t | () | :maybe}

	 If #t then compile into interpreted byte code instead of machine
	 instructions.  If () compile to machine code.  If :MAYBE, then only
	 byte-compile when SPEED is 0 and DEBUG <= 1.

	 Byte code is several times smaller than machine code, and much
	 slower.

     $print

	 If #t then set *compile-print* so that the compiler prints to error
	 output when each function is compiled.

     $progress

	 If #t, then set *compile-progress* so that the compiler prints
	 progress information to error output about the phases of
	 compilation of each function.  This is useful in large block
	 compilations."
  (let* ((fasl-file nil)
	 (error-file-stream nil)
	 (output-file-name nil)
	 (*compiler-error-output* *compiler-error-output*)
	 (*compiler-trace-output* nil)
	 (compile-won nil)
	 (error-severity nil)
	 (source (verify-source-files source))
	 (source-info (make-file-source-info source))
	 (default (pathname (first source))))
    (unwind-protect
	(progn
	  (flet ((frob (file type)
		   (if (eq file t)
		       (make-pathname :type type
				      :defaults
				      (if (logical-pathname-p default)
					  (translate-logical-pathname default)
					  default))
		       (pathname file))))

	    (when output-file
	      (setq output-file-name
		    (compile-file-pathname (first source)
					   :output-file output-file
					   :byte-compile *byte-compile*))
	      (setq fasl-file (open-fasl-file output-file-name
					      (namestring (first source))
					      (eq *byte-compile* t))))

	    (when trace-file
	      (setq *compiler-trace-output*
		    (open (frob trace-file "trace")
			  :if-exists :supersede
			  :direction :output)))

	    (when error-file
	      (setq error-file-stream
		    (open (frob error-file "err")
			  :if-exists :supersede
			  :direction :output))))

	  (setq *compiler-error-output*
		(apply #'make-broadcast-stream
		       (remove nil
			       (list (if (eq error-output t)
					 *error-output*
					 error-output)
				     error-file-stream))))

	  (if *compile-verbose*
	      (start-error-output source-info))
	  (setq error-severity
		(let ((*compile-object* fasl-file))
		  (sub-compile-file source-info)))
	  (setq compile-won t))

      (close-source-info source-info)

      (when fasl-file
	(close-fasl-file fasl-file (not compile-won))
	(setq output-file-name (pathname (fasl-file-stream fasl-file)))
	(when (and compile-won *compile-verbose*)
	  (compiler-mumble "~2&~A written.~%" (namestring output-file-name))))

      (when *compile-verbose*
	(finish-error-output source-info compile-won))

      (when error-file-stream
	(let ((name (pathname error-file-stream)))
	  ;;
	  ;; Leave this var pointing to something reasonable in case someone
	  ;; tries to use it before the LET ends, e.g. during the LOAD.
	  (setq *compiler-error-output* *error-output*)
	  (close error-file-stream)
	  (when (and compile-won (not error-severity))
	    (delete-file name))))

      (when *compiler-trace-output*
	(close *compiler-trace-output*)))

    (when load
      (or output-file
	  (error "Need an output file in order to :LOAD."))
      (load output-file-name :verbose *compile-verbose*))

    (values (if output-file
		;; Hack around filesystem race condition...
		(or (probe-file output-file-name) output-file-name)
		nil)
	    (not (null error-severity))
	    (if (member error-severity '(:warning :error)) t nil))))


;;;; COMPILE and UNCOMPILE.

;;; GET-LAMBDA-TO-COMPILE  --  Internal
;;;
(defun get-lambda-to-compile (definition)
  (if (consp definition)
      definition
      (multiple-value-bind (def env-p)
			   (function-lambda-expression definition)
	(if env-p
	    (error "~S was defined in a non-null environment." definition))
	(or def
	    (error "Can't find a definition for ~S." definition))
	def)))

;;; COMPILE-FIX-FUNCTION-NAME  --  Internal
;;;
;;; Find the function that is being compiled by COMPILE and bash its name
;;; to NAME.  We also substitute for any references to name so that
;;; recursive calls will be compiled direct.  Lambda is the top-level
;;; lambda for the compilation.  A REF for the real function is the only
;;; thing in the top-level lambda other than the bind and return, so it
;;; isn't too hard to find.
;;;
(defun compile-fix-function-name (lambda name)
  (declare (type clambda lambda) (type (or symbol cons) name))
  (when name
    (let ((fun (ref-leaf
		(continuation-next
		 (node-cont (lambda-bind lambda))))))
      (setf (leaf-name fun) name)
      (let ((old (gethash name *free-functions*)))
	(when old (substitute-leaf fun old)))
      name)))

;;; COMPILE  --  Public
;;;
;;; FIX :print :verbose :progress
;;;
(defun compile (name &optional (definition (fdefinition name)))
  "Compile the function whose name is $name.  If $definition is true
   compile it as a lambda expression and place it in the function cell of
   $name.  $definition may also be an interpreted function.

   Return as first value the function name, or the function object if $name
   is ().  Return the second value true only if compiler diagnostics were
   issued.  Return the third value true if the only compiler diagnostics
   issued were style warnings.  That is, a true third value indicates that
   there were \"serious\" compiler diagnostics issued, or that a condition
   of type error or warning (other than style-warning) was signalled during
   compilation."
  (with-compilation-unit ()
    (with-ir1-namespace
      (let* ((*backend* *native-backend*)
	     (*info-environment*
	      (or (backend-info-environment *backend*)
		  *info-environment*))
	     (start-errors *compiler-error-count*)
	     (start-warnings *compiler-warning-count*)
	     (start-notes *compiler-note-count*)
	     (*lexical-environment* (make-null-environment))
	     (form `#',(get-lambda-to-compile definition))
	     (*source-info* (make-lisp-source-info form))
	     (*top-level-lambdas* ())
	     (*converting-for-interpreter* nil)
	     (*block-compile* nil)
	     (*compiler-error-bailout*
	      #'(lambda ()
		  (compiler-mumble
		   "~2&Fatal error, aborting compilation...~%")
		  (return-from compile (values nil t nil))))
	     (*compiler-error-output* *error-output*)
	     (*compiler-trace-output* nil)
	     (*current-path* nil)
	     (*last-source-context* nil)
	     (*last-original-source* nil)
	     (*last-source-form* nil)
	     (*last-format-string* nil)
	     (*last-format-args* nil)
	     (*last-message-count* 0)
	     (*compile-object* (make-core-object))
	     (*gensym-counter* 0))
	(with-debug-counters
	  (clear-stuff)  ; FIX clear-globals?
	  (find-source-paths form 0)
	  (let ((lambda (ir1-top-level form '(original-source-start 0 0) t)))

	    (compile-fix-function-name lambda name)
	    (let* ((component
		    (block-component (node-block (lambda-bind lambda))))
		   (*all-components* (list component)))
	      (local-call-analyze component))

	    (multiple-value-bind (components top-components)
		(find-initial-dfo (list lambda))
	      (let ((*all-components* (append components top-components)))
		(dolist (component *all-components*)
		  (compile-component component))))

	    (let* ((res (core-call-top-level-lambda lambda *compile-object*))
		   (return (or name res)))
	      (fix-core-source-info *source-info* *compile-object* res)
	      (if name
		  (setf (fdefinition name) res))

	      (cond ((or (> *compiler-error-count* start-errors)
			 (> *compiler-warning-count* start-warnings))
		     (values return t t))
		    ((> *compiler-note-count* start-notes)
		     (values return t nil))
		    (t
		     (values return nil nil))))))))))

;;; UNCOMPILE  --  Public
;;;
(defun uncompile (name)
  "Attempt to replace $name's definition with an interpreted version of
   that definition.  If no interpreted definition is to be found, then
   signal an error."
  (let ((def (fdefinition name)))
    (if (eval:interpreted-function-p def)
	(warn "~S is already interpreted." name)
	(setf (fdefinition name)
	      (coerce (get-lambda-to-compile def) 'function))))
  name)


;;; COMPILE-FILE-PATHNAME  --  Public
;;;
(defun compile-file-pathname (file-path &key (output-file t) byte-compile
					&allow-other-keys)
  "Return a pathname describing what file COMPILE-FILE would write to given
   these arguments."
  (declare (values (or null pathname)))
  (let ((pathname (pathname file-path)))
    (cond ((not (eq output-file t))
	   (when output-file
	     (translate-logical-pathname (pathname output-file))))
	  ((and (logical-pathname-p pathname) (not (eq byte-compile t)))
	   (make-pathname :type "FASL" :defaults pathname
			  :case :common))
	  (t
	   (make-pathname :defaults (translate-logical-pathname pathname)
			  :type (if (eq byte-compile t)
				    (backend-byte-fasl-file-type *backend*)
				    (backend-fasl-file-type *backend*)))))))


#[ Compiler

[ Compiler Introduction                      ]
[ Calling the Compiler                       ]
[ Compilation Units                          ]
[ Interpreting Error Messages                ]
[ Types in the Compiler                      ]
[ Getting Existing Programs to Run           ]
[ Compiler Policy                            ]
[ Open Coding and Inline Expansion           ]
[ Advanced Compiler Use and Efficiency Hints ]
[ Internal Design                            ]  of the compiler and run-time system.
]#

#[ Compiler Introduction

The compiler has many features:

  * Source level debugging of compiled code (described in [Debugger].)

  * Type error compiler warnings for type errors detectable at compile time.

  * Compiler error messages that provide a good indication of where the
    error appeared in the source.

  * Full run-time checking of all potential type errors, with optimization
    of type checks to minimize the cost.

  * Scheme-like features such as proper tail recursion and extensive
    source-level optimization.

  * Advanced tuning and optimization features such as comprehensive
    efficiency notes, flow analysis, and untagged number representations
    (described in [Advanced Compiler Use and Efficiency Hints]).
]#

#[ Calling the Compiler
\cindex{compiling}

Functions may be compiled using `compile', `compile-file', or
`compile-from-stream'.

{function:compile}
{function:compile-file}

{variable:*compile-verbose*}
{variable:*compile-print*}
{variable:*compile-progress*}

{function:ext:compile-from-stream}
]#

#[ Compilation Units
\cpsubindex{compilation}{units}

The `with-compilation-unit' macro provides a mechanism for eliminating
spurious undefined warnings when there are forward references across files,
and also provides a standard way to access compiler extensions.

{function:with-compilation-unit}

[ Undefined Warnings ]
]#

#[ Interpreting Error Messages
\cpsubindex{error messages}{compiler}
\cindex{compiler error messages}

One of the compiler's unique features is the level of source location
information it provides in error messages.  The error messages contain a
lot of detail in a terse format, to they may be confusing at first.  Error
messages will be illustrated using this example program:
    (defmacro zoq (x)
      `(roq (ploq (+ ,x 3))))

    (defun foo (y)
      (declare (symbol y))
      (zoq y))

The main problem with this program is that it is trying to add \code{3} to a
symbol.  Note also that the functions \code{roq} and \code{ploq} aren't defined
anywhere.

[ The Parts of the Error Message ]
[ The Original and Actual Source ]
[ The Processing Path            ]
[ Error Severity                 ]
[ Errors During Macroexpansion   ]
[ Read Errors                    ]
[ Error Message Parameterization ]
]#

#[ The Parts of the Error Message

The compiler will produce this warning:
    File: /usr/me/stuff.lisp

    In: DEFUN FOO
      (ZOQ Y)
    --> ROQ PLOQ +
    ==>
      Y
    Warning: Result is a SYMBOL, not a NUMBER.

In this example we see each of the six possible parts of a compiler error
message:

  % File: /usr/me/stuff.lisp

    This is the \var{file} that the compiler read the relevant code from.
    The file name is displayed because it may not be immediately obvious
    when there is an error during compilation of a large system, especially
    when \code{with-compilation-unit} is used to delay undefined warnings.}

  % In: DEFUN FOO

    This is the \var{definition} or top-level form responsible for the
    error.  It is obtained by taking the first two elements of the
    enclosing form whose first element is a symbol beginning with
    "\code{DEF}".  If there is no enclosing \w{\var{def}mumble}, then the
    outermost form is used.  If there are multiple \w{\var{def}mumbles},
    then they are all printed from the out in, separated by \code{=>}'s.
    In this example, the problem was in the \code{defun} for \code{foo}.

  % (ZOQ Y)

    This is the original source form responsible for the error.
    Original source means that the form directly appeared in the original
    input to the compiler, i.e. in the lambda passed to \code{compile} or
    the top-level form read from the source file.  In this example, the
    expansion of the \code{zoq} macro was responsible for the error.

  % --> ROQ PLOQ +

    This is the \i{processing path} that the compiler used to produce the
    errorful code.  The processing path is a representation of the
    evaluated forms enclosing the actual source that the compiler
    encountered when processing the original source.  The path is the first
    element of each form, or the form itself if the form is not a list.
    These forms result from the expansion of macros or source-to-source
    transformation done by the compiler.  In this example, the enclosing
    evaluated forms are the calls to \code{roq}, \code{ploq} and \code{+}.
    These calls resulted from the expansion of the \code{zoq} macro.

  % ==>  Y

    This is the actual source responsible for the error.  If the actual
    source appears in the explanation, then we print the next enclosing
    evaluated form, instead of printing the actual source twice.  (This is
    the form that would otherwise have been the last form of the processing
    path.)  In this example, the problem is with the evaluation of the
    reference to the variable \code{y}.

  % Warning: Result is a SYMBOL, not a NUMBER.

    This is the \var{explanation} the problem.  In this example, the
    problem is that \code{y} evaluates to a \code{symbol}, but is in a
    context where a number is required (the argument to \code{+}).

Note that each part of the error message is distinctively marked:

  * \code{File:} and \code{In:} mark the file and definition, respectively.

  * The original source is an indented form with no prefix.

  * Each line of the processing path is prefixed with \code{-->}.

  * The actual source form is indented like the original source, but is marked by a
    preceding \code{==>} line.

  * The explanation is prefixed with the error severity
    (\pxlref{error-severity}), either \code{Error:}, \code{Warning:}, or
    \code{Note:}.

Each part of the error message is more specific than the preceding one.  If
consecutive error messages are for nearby locations, then the front part of
the error messages would be the same.  In this case, the compiler omits as
much of the second message as in common with the first.  For example:

    File: /usr/me/stuff.lisp

    In: DEFUN FOO
      (ZOQ Y)
    --> ROQ
    ==>
      (PLOQ (+ Y 3))
    Warning: Undefined function: PLOQ

    ==>
      (ROQ (PLOQ (+ Y 3)))
    Warning: Undefined function: ROQ

In this example, the file, definition and original source are identical for
the two messages, so the compiler omits them in the second message.  If
consecutive messages are entirely identical, then the compiler prints only
the first message, followed by:

    [Last message occurs <repeats> times]

where "<repeats>" is the number of times the message was given.

A file line is only printed if the source was from a file.  If the actual
source is the same as the original source, then the processing path and
actual source will be left out.  If no forms intervene between the original
source and the actual source, then the processing path will also be
omitted.
]#

#[ The Original and Actual Source
\cindex{original source}
\cindex{actual source}

The original source displayed will almost always be a list.  If the actual
source for an error message is a symbol, the original source will be the
immediately enclosing evaluated list form.  So even if the offending symbol
does appear in the original source, the compiler will print the enclosing
list and then print the symbol as the actual source (as though the symbol
were introduced by a macro.)

When the actual source is displayed (and is not a symbol), it will always
be code that resulted from the expansion of a macro or a source-to-source
compiler optimization.  This is code that did not appear in the original
source program; it was introduced by the compiler.

Keep in mind that when the compiler displays a source form in an error
message, it always displays the most specific (innermost) responsible form.
For example, compiling this function:
    (defun bar (x)
      (let (a)
        (declare (fixnum a))
        (setq a (foo x))
        a))

Gives this error message:

    In: DEFUN BAR
      (LET (A) (DECLARE (FIXNUM A)) (SETQ A (FOO X)) A)
    Warning: The binding of A is not a FIXNUM:
      NIL

This error message is saying that there is a problem with the `let' itself.
In this example, the problem is that the initial value of "a" should be a
fixnum.
]#

#[ The Processing Path
\cindex{processing path}
\cindex{macroexpansion}
\cindex{source-to-source transformation}

The processing path is mainly useful for inspecting the execution of
macros.  Consider this example:

    (defun foo (n)
      (dotimes (i n *undefined*)))

Compiling results in this error message:

    In: DEFUN FOO
      (DOTIMES (I N *UNDEFINED*))
    --> DO BLOCK LET TAGBODY RETURN-FROM
    ==>
      (PROGN *UNDEFINED*)
    Warning: Undefined variable: *UNDEFINED*

Note that `do' appears in the processing path.  This is because `dotimes'
expands into:

    (do ((i 0 (1+ i)) (#:g1 n))
        ((>= i #:g1) *undefined*)
      (declare (type unsigned-byte i)))

The rest of the processing path results from the expansion of `do':

    (block nil
      (let ((i 0) (#:g1 n))
        (declare (type unsigned-byte i))
        (tagbody (go #:g3)
         #:g2    (psetq i (1+ i))
         #:g3    (unless (>= i #:g1) (go #:g2))
                 (return-from nil (progn *undefined*)))))

In this example, the compiler descended into the `block', `let', `tagbody'
and `return-from' to reach the `progn' printed as the actual source.  This
is a place where the "actual source appears in explanation" rule was
applied.  The innermost actual source form was the symbol `*undefined*'
itself, but that also appeared in the explanation, so the compiler backed
out one level.
]#

#[ Error Severity
\cindex{severity of compiler errors}
\cindex{compiler error severity}

There are three levels of compiler error severity:

  % Error

    This severity is used when the compiler encounters a problem serious
    enough to prevent normal processing of a form.  Instead of compiling
    the form, the compiler compiles a call to `error'.  Errors are used
    mainly for signalling syntax errors.  If an error happens during
    macroexpansion, the compiler will handle it.  The compiler also handles
    and attempts to proceed from read errors.

  % Warning

    Warnings are used when the compiler can prove that something bad will
    happen if a portion of the program is executed, but the compiler can
    proceed by compiling code that signals an error at runtime if the problem
    has not been fixed:

     * Violation of type declarations, or

     * Function calls that have the wrong number of arguments or malformed
       keyword argument lists, or

     * Referencing a variable declared \code{ignore}, or unrecognized
       declaration specifiers.

  % Note

    Notes are used when there is something that seems a bit odd, but that might
    reasonably appear in correct programs.
]#

#[ Errors During Macroexpansion
\cpsubindex{macroexpansion}{errors during}

The compiler handles errors that happen during macroexpansion, turning them
into compiler errors.  If you want to debug the error (to debug a macro),
you can set \code{*break-on-signals*} to \code{error}.  For example, this
definition:
    (defun foo (e l)
      (do ((current l (cdr current))
           ((atom current) nil))
          (when (eq (car current) e) (return current))))

gives this error:

    In: DEFUN FOO
      (DO ((CURRENT L #) (# NIL)) (WHEN (EQ # E) (RETURN CURRENT)) )
    Error: (during macroexpansion)

    Error in function LISP ]DO-DO-BODY.
    DO step variable is not a symbol: (ATOM CURRENT)
]#

#[ Read Errors
\cpsubindex{read errors}{compiler}

The compiler also handles errors while reading the source.  For example:
    Error: Read error at 2:
     "(,/\back foo)"
    Error in function LISP ]COMMA-MACRO.
    Comma not inside a backquote.

The "\code{at 2}" refers to the character position in the source file at
which the error was signalled, which is generally immediately after the
erroneous text.  The next line, "(,/\back foo)", is the line in the source
that contains the error file position.  The "/\back " indicates the error
position within that line (in this example, immediately after the offending
comma).

In the editor the sequence "meta-g k <position> enter" goes to character
number "position".  If the source is from an editor buffer, then the
position is relative to the start of the compiled region or `defun',
instead of the file or buffer start.

After printing a read error message, the compiler attempts to recover from
the error by backing up to the start of the enclosing top-level form and
reading again with *read-suppress* true.  If the compiler can recover from
the error, then it substitutes a call to `cerror' for the unreadable form
and proceeds to compile the rest of the file normally.

If there is a read error when the file position is at the end of the file
(i.e., an unexpected EOF error), then the error message looks like this:

    Error: Read error in form starting at 14:
     "(defun test ()"
    Error in function LISP ]FLUSH-WHITESPACE.
    EOF while reading #<Stream for file "/usr/me/test.lisp">

In this case, "starting at 14" indicates the character position at which
the compiler started reading, i.e. the position before the start of the
form that was missing the closing delimiter.  The line "(defun test ()" is
first line after the starting position that the compiler thinks might
contain the unmatched open delimiter.
]#

#[ Error Message Parameterization
\cpsubindex{error messages}{verbosity}
\cpsubindex{verbosity}{of error messages}

There is some control over the verbosity of error messages.  The variables
*undefined-warning-limit*, *efficiency-note-limit* and
*efficiency-note-cost-threshold* may also by useful.

{variable:*enclosing-source-cutoff*}
{variable:*error-print-length*}
{variable:*error-print-level*}

{function:def-source-context}
]#

#[ Types in the Compiler
\cpsubindex{types}{in the compiler}

A big difference between the compiler and all other \llisp{} compilers
is the approach to type checking and amount of knowledge about types:

  * the compiler treats type declaration as assertions about the program
    that it must checked.

  * the compiler has a great knowledge of the type system.  Support is
    lacking only for the \code{not}, \code{and} and \code{satisfies} types.

See also sections \ref{advanced-type-stuff} and \ref{type-inference}.

[ Compile Time Type Errors ]
[ Precise Type Checking    ]
[ Weakened Type Checking   ]
]#

#[ Compile Time Type Errors
\cindex{compile time type errors}
\cpsubindex{type checking}{at compile time}

If the compiler can prove at compile time that some portion of the program
cannot be executed without a type error, then it will give a warning at
compile time.  It is possible that the offending code would never actually
be executed at run-time due to some higher level consistency constraint
unknown to the compiler, so a type warning doesn't always indicate an
incorrect program.  For example, consider this code fragment:
    (defun raz (foo)
      (let ((x (case foo
                 (:this 13)
                 (:that 9)
                 (:the-other 42))))
        (declare (fixnum x))
        (foo x)))

Compilation produces this warning:

    In: DEFUN RAZ
      (CASE FOO (:THIS 13) (:THAT 9) (:THE-OTHER 42))
    --> LET COND IF COND IF COND IF
    ==>
      (COND)
    Warning: This is not a FIXNUM:
      NIL

In this case, the warning is telling you that if \code{foo} isn't any of
\code{:this}, \code{:that} or \code{:the-other}, then \code{x} will be
initialized to \false, which the \code{fixnum} declaration makes illegal.
The warning will go away if \code{ecase} is used instead of \code{case}, or
if \code{:the-other} is changed to \true.

This sort of spurious type warning happens moderately often in the
expansion of complex macros and in inline functions.  In such cases, there
may be dead code that is impossible to correctly execute.  The compiler
can't always prove this code is dead (could never be executed), so it
compiles the erroneous code (which will always signal an error if it is
executed) and gives a warning.

{function:ext:required-argument}

Type warnings are suppressed when the \code{extensions:inhibit-warnings}
optimization quality is \code{3} (\pxlref{compiler-policy}.)  This can be
used in a local declaration to inhibit type warnings in a code fragment
that has spurious warnings.
]#

#[ Precise Type Checking
\cindex{precise type checking}
\cpsubindex{type checking}{precise}

With the default compilation policy, all type assertions are precisely
checked.  Precise checking means that the check is done as though `typep'
had been called with the exact type specifier that appeared in the
declaration.  The compiler uses policy to determine whether to trust type
assertions (\pxlref{compiler-policy}).  Type assertions from declarations
are the same as the type assertions on arguments to built-in functions.
Adding type declarations makes code safer.

If a variable is declared to be (integer 3 17), then its value must always
always be an integer between 3 and 17.  If multiple type declarations apply
to a single variable, then all the declarations must be correct; it is as
though all the types were intersected producing a single \"and"\ type
specifier.

Argument type declarations are automatically enforced.  If you declare the
type of a function argument, a type check will be done when that function
is called.  In a function call, the called function does the argument type
checking, which means that a more restrictive type assertion in the calling
function (e.g., from \code{the}) may be lost.

The types of structure slots are also checked.  The value of a structure
slot must always be of the type indicated in any \code{:type} slot option.
Because of precise type checking, the arguments to slot accessors are
checked to be the correct type of structure.

Declaring the types of function arguments and structure slots as precisely
as possible maximizes the benefit of the compiler type checking.  This
often involves the use of \code{or}, \code{member} and other list-style
type specifiers.  Paradoxically, even though adding type declarations
introduces type checks, it usually reduces the overall amount of type
checking.  This is especially true for structure slot type declarations.

The compiler uses the \code{safety} optimization quality (rather than
presence or absence of declarations) to choose one of three levels of
run-time type error checking: \pxlref{optimize-declaration}.
\xlref{advanced-type-stuff} details types in the compiler further.
]#

#[ Weakened Type Checking
\cindex{weakened type checking}
\cpsubindex{type checking}{weakened}

When the value for the \code{speed} optimization quality is greater than
\code{safety}, and \code{safety} is not \code{0}, then type checking is
weakened to reduce the speed and space penalty.  In structure-intensive
code this can double the speed, yet still catch most type errors.

A type check is weakened by changing the check to be for some
convenient supertype of the asserted type.  For example,
(integer 3 17) is changed to fixnum,
(simple-vector 17) to simple-vector, and structure
types are changed to structure.  A complex check like:

    FIX

will be omitted entirely (i.e., the check is weakened to \code{*}.)  If a
precise check can be done at the same cost, then precise type checking
remains.

Although weakened type checking is similar to type checking done by other
compilers, it is sometimes safer and sometimes less safe.  Weakened checks are
done in the same places as precise checks, so all the preceding discussion
about where checking is done still applies.  Weakened checking is sometimes
somewhat unsafe because although the check is weakened, the precise type is
still input into type inference.  In some contexts this will result in type
inferences not justified by the weakened check, and hence deletion of some type
checks that would be done by conventional compilers.

For example, if this code was compiled with weakened checks:
(defstruct foo
  (a nil :type simple-string))

(defstruct bar
  (a nil :type single-float))

(defun myfun (x)
  (declare (type bar x))
  (* (bar-a x) 3.0))
and `myfun' was passed a \code{foo}, then no type error would be
signalled, and we would try to multiply a \code{simple-vector} as though it
were a float (with unpredictable results.)  This is because the check for
\code{bar} was weakened to \code{structure}, yet when compiling the call to
\code{bar-a}, the compiler assumes it has a \code{bar}.

Note that normally even weakened type checks report the precise type in error
messages.  For example, if \code{myfun}'s \code{bar} check is weakened to
\code{structure}, and the argument is (), then the error will be:

Type-error in MYFUN:
  NIL is not of type BAR

However, there is some speed and space cost for signalling a precise error, so
the weakened type is reported if the \code{speed} optimization quality is 3 or
\code{debug} quality is less than 1:

Type-error in MYFUN:
  NIL is not of type STRUCTURE

\xlref{optimize-declaration} discusses the \code{optimize} declaration
further.
]#

#[ Getting Existing Programs to Run
\cpsubindex{existing programs}{to run}
\cpsubindex{types}{portability}
 are mostly
incorrect declarations, although compile-time type errors can find actual
bugs if parts of the program have never been tested.

Some declaration errors can only be detected by run-time type checking.  It
is very important to initially compile programs with full type checks and
then test that version.  After the checking version has been tested, then
type checks can be considered for weakening or forfeight.  This applies
even to previously tested programs.

== Initial variable values ==

The most common problem is with variables whose initial value doesn't match the
type declaration.  Incorrect initial values will always be flagged by a
compile-time type error, and they are simple to fix once located.  Consider
this code fragment:
    (prog (foo)
      (declare (fixnum foo))
      (setq foo ...)
      ...)

Here the variable \code{foo} is given an initial value of \false, but is declared
to be a \code{fixnum}.  Even if it is never read, the initial value of a variable
must match the declared type.  There are two ways to fix this problem.  Change
the declaration:
    (prog (foo)
      (declare (type (or fixnum null) foo))
      (setq foo ...)
      ...)

or change the initial value:

    (prog ((foo 0))
      (declare (fixnum foo))
      (setq foo ...)
      ...)

It is generally preferable to change to a legal initial value rather than
to weaken the declaration.  However, sometimes it is simpler to weaken the
declaration than to try to make an initial value of the appropriate type.

== `defmacro' arguments ==

Another declaration problem occasionally encountered is incorrect
declarations on `defmacro' arguments.  This probably usually happens when a
function is converted into a macro.  Consider this macro:

    (defmacro my-1+ (x)
      (declare (fixnum x))
      `(the fixnum (1+ ,x)))

Although a legal and well-defined macro, the meaning of this definition is
almost certainly not what the writer intended.  For example, this call is
illegal:

    (my-1+ (+ 4 5))

The call is an error because the argument to the macro is (+ 4 5), which is
a list, and it should be a fixnum.  Because of macro semantics, it is
hardly ever useful to declare the types of macro arguments.  If ther must
be an assertion about the type of the result of evaluating a macro
argument, then a `the' can be inserted into the expansion:

    (defmacro my-1+ (x)
      `(the fixnum (1+ (the fixnum ,x))))

In this case, it would be stylistically preferable to change the macro back
to a function and declare it inline.  Macros and inline functions are equal
in efficiency.  \xlref{inline-expansion}.

== Subtle errors ==

Some more subtle problems are caused by errorneous declarations that can't
be detected at compile time.  Consider this code:

    (do ((pos 0 (position #\back a string :start (1+ pos))))
        ((null pos))
      (declare (fixnum pos))
      ...)

Although \code{pos} is almost always a \code{fixnum}, it is () at the end
of the loop.  If this example is compiled with full type checks, then
running it will signal a type error at the end of the loop.  If compiled
without type checks, the program will go into an infinite loop (or perhaps
\code{position} will complain because \w{\code{(1+ nil)}} isn't a sensible
start.)  Why?  Because if you compile without type checks, the compiler
just quietly believes the type declaration.  Since \code{pos} is always a
\code{fixnum}, it is never \nil, so (null pos) is always false, and the
loop exit test is optimized away.  Such errors are sometimes flagged by
unreachable code notes (\pxlref{dead-code-notes}), but it is still
important to initially compile any system with full type checks.

In this case, the fix is to weaken the type declaration to (or fixnum
null). (Actually, this declaration is totally unnecessary in the compiler,
since it already knows \code{position} returns a non-negative \code{fixnum}
or ().)  Note that there is usually little performance penalty for
weakening a declaration in this way.  Any numeric operations in the body
can still assume the variable is a \code{fixnum}, since () is not a
legal numeric argument.  Another possible fix would be to say:

    (do ((pos 0 (position #\back a string :start (1+ pos))))
        ((null pos))
      (let ((pos pos))
        (declare (fixnum pos))
        ...))

This would be preferable in some circumstances, since it would allow a
non-standard representation to be used for the local \code{pos} variable in the
loop body (see section \ref{ND-variables}.)

In summary, all values that a variable ever has must be of the declared
type, and initial tests should use safe code.
]#

#[ Compiler Policy
\cpsubindex{policy}{compiler}
\cindex{compiler policy}

The policy determines how the compiler compiles a program.  The policy is
logically (and often textually) separate from the program itself.  Broad
control of policy is provided by the \code{optimize} declaration; other
declarations and variables control more specific aspects of compilation.

[ The Optimize Declaration           ]
[ The Optimize-Interface Declaration ]
]#

#[ The Optimize Declaration
\cindex{optimize declaration}
\cpsubindex{declarations}{\code{optimize}}

The \code{optimize} declaration recognizes six different qualities.  The
qualities are conceptually independent aspects of program performance.  In
reality, increasing one quality tends to have adverse effects on other
qualities.  The compiler compares the relative values of qualities when it
needs to make a trade-off; i.e., if \code{speed} is greater than
\code{safety}, then improve speed at the cost of safety.

The default for all qualities (other than \code{debug}) is 1.  Whenever
qualities are equal, ties are broken according to a broad idea of what a
good default environment is supposed to be.  Generally this downplays
\code{speed}, \code{compile-speed} and \code{space} in favor of
\code{safety} and \code{debug}.  Novice and casual users should stick to
the given policy.  Advanced users often want to improve speed and memory
usage at the cost of safety and debuggability.

If the value for a quality is 0 or 3, then it may have a special
interpretation.  A value of 0 means "totally unimportant", and a 3 means
"ultimately important."  These extreme optimization values enable "heroic"
compilation strategies that are not always desirable and sometimes
self-defeating.  Specifying more than one quality as 3 is not desirable,
since it doesn't tell the compiler which quality is most important.

These are the optimization qualities:

  % speed

    \cindex{speed optimization quality}

    How fast the program should is run.  \code{speed 3} enables some
    optimizations that hurt debuggability.

  % compilation-speed

    \cindex{compilation-speed optimization quality}

    How fast the compiler should run.  Note that increasing this above
    \code{safety} weakens type checking.

  % space

    \cindex{space optimization quality}

    How much space the compiled code should take up.  Inline expansion is
    mostly inhibited when \code{space} is greater than \code{speed}.  A
    value of 0 enables promiscuous inline expansion.  Wide use of a 0 value
    is not recommended, as it may waste so much space that run time is
    slowed.  \xlref{inline-expansion} discusses inline expansion.

  % debug

    \cindex{debug optimization quality}

    How debuggable the program should be.  The quality is treated
    differently from the other qualities: each value indicates a particular
    level of debugger information; it is not compared with the other
    qualities.  \xlref{debugger-policy} for more details.

  % safety

    \cindex{safety optimization quality}

    How much error checking should be done.  If \code{speed}, \code{space}
    or \code{compilation-speed} is more important than \code{safety}, then
    type checking is weakened (\pxlref{weakened-type-checks}).  If
    \code{safety} if \code{0}, then no run time error checking is done.  In
    addition to suppressing type checks, \code{0} also suppresses argument
    count checking, unbound-symbol checking and array bounds checks.

  % extensions:inhibit-warnings

    \cindex{inhibit-warnings optimization quality}

    How little (or how much) diagnostic output should be printed during
    compilation.  This quality is compared to other qualities to determine
    whether to print style notes and warnings concerning those qualities.
    If \code{speed} is greater than \code{inhibit-warnings}, then notes
    about how to improve speed will be printed, etc.  The default value is
    1, so raising the value for any standard quality above its default
    enables notes for that quality.  If \code{inhibit-warnings} is 3, then
    all notes and most non-serious warnings are inhibited.  This is useful
    with \code{declare} to suppress warnings about unavoidable problems.
]#

#[ The Optimize-Interface Declaration
\cindex{optimize-interface declaration}
\cpsubindex{declarations}{\code{optimize-interface}}

The \code{extensions:optimize-interface} declaration is identical in syntax to the
\code{optimize} declaration, but it specifies the policy used during compilation
of code the compiler automatically generates to check the number and type of
arguments supplied to a function.  It is useful to specify this policy
separately, since even thoroughly debugged functions are vulnerable to being
passed the wrong arguments.  The \code{optimize-interface} declaration can specify
that arguments should be checked even when the general \code{optimize} policy is
unsafe.

Note that this argument checking is the checking of user-supplied arguments
to any functions defined within the scope of the declaration, \code{not}
the checking of arguments to Lisp primitives that appear in those
definitions.

The idea behind this declaration is that it allows the definition of functions
that appear fully safe to other callers, but that do no internal error
checking.  Of course, it is possible that arguments may be invalid in ways
other than having incorrect type.  Functions compiled unsafely must still
protect themselves against things like user-supplied array indices that are out
of bounds and improper lists.  See also the \kwd{context-declarations} option
to `with-compilation-unit'.
]#

#[ Open Coding and Inline Expansion
\cindex{open-coding}
\cindex{inline expansion}
\cindex{static functions}

Since Lisp forbids the redefinition of standard functions, the compiler can
have special knowledge of these standard functions embedded in it.  This
special knowledge is used in various ways (open coding, inline expansion,
source transformation), but the implications to the user are basically the
same:

  * Attempts to redefine standard functions may be frustrated, since the
    function may never be called.  Although it is technically illegal to
    redefine standard functions, users sometimes want to implicitly redefine
    these functions when they are debugging using the `trace' macro.
    Special-casing of standard functions can be inhibited using the
    \code{notinline} declaration.

  * The compiler can have multiple alternate implementations of standard
    functions that implement different trade-offs of speed, space and
    safety.  This selection is based on the compiler policy,
    \pxlref{compiler-policy}.

When a function call is open coded, inline code whose effect is equivalent
to the function call is substituted for that function call.  When a
function call is closed coded, it is usually left as is, although it might
be turned into a call to a different function with different arguments.  As
an example, if `nthcdr' were to be open coded, then

    (nthcdr 4 foobar)

might turn into

    (cdr (cdr (cdr (cdr foobar))))

or even

    (do ((i 0 (1+ i))
         (list foobar (cdr foobar)))
        ((= i 4) list))

If `nth' is closed coded, then

(nth x l)

might stay the same, or turn into something like:

    (car (nthcdr x l))

In general, open coding sacrifices space for speed, but some functions
(such as `car') are so simple that they are always open-coded.  Even when
not open-coded, a call to a standard function may be transformed into a
different function call (as in the last example) or compiled as \i{static
call}.  Static function call uses a more efficient calling convention that
forbids redefinition.
]#


#[ Advanced Compiler Use and Efficiency Hints

[ Advanced Compiler Introduction   ]
[ More About Types in the Compiler ]
[ Type Inference                   ]
[ Source Optimization              ]
[ Tail Recursion                   ]
[ Local Call                       ]
[ Block Compilation                ]
[ Inline Expansion                 ]
[ Byte Coded Compilation           ]
[ Object Representation            ]
[ Numbers                          ]
[ General Efficiency Hints         ]
[ Efficiency Notes                 ]
[ The Profiler                     ]
]#

#[ Advanced Compiler Introduction

The path to efficient code starts with good algorithms and sensible
programming techniques.  Combined with this the programmer needs to know
the compiler implementation's quirks and features.

This section is mostly a fairly long and detailed overview of what
optimizations the compiler does.  Although there are negative suggestions
of features to be aware of, the main emphasis is on describing features
that programmers can count on being efficient.

The optimizations described here can have the effect of speeding up
existing programs written in conventional styles, but the potential for new
programming styles that are clearer and less error-prone is at least as
significant.  For this reason, several sections end with a discussion of
the implications of these optimizations for programming style.

[ Types                     ]
[ Optimization              ]
[ Function Call             ]
[ Representation of Objects ]
[ Writing Efficient Code    ]
]#

#[ Types

The compiler support for types is notable in three major ways:

  * Precise type checking encourages the specific use of type declarations
    as a form of run-time consistency checking.  This speeds development by
    localizing type errors and giving more meaningful error messages.
    \xlref{precise-type-checks}.  The compiler produces completely safe code;
    optimized type checking maintains reasonable efficiency on conventional
    hardware (\pxlref{type-check-optimization}.)

  * Comprehensive support for the type system makes complex type specifiers
    useful.  Using type specifiers such as \code{or} and \code{member} has
    both efficiency and robustness advantages.  \xlref{advanced-type-stuff}.

  * Type inference eliminates the need for some declarations, and also aids
    compile-time detection of type errors.  Given detailed type
    declarations, type inference can often eliminate type checks and enable
    more efficient object representations and code sequences.  Checking all
    types results in fewer type checks.  See sections \ref{type-inference}
    and \ref{non-descriptor}.
]#

#[ Optimization

The main barrier to efficient Lisp programs is that it can be hard to
arrive at the efficient way to code the program.  Lisp is a highly complex
language, and usually has many semantically equivalent "reasonable" ways to
code a given problem.  It is desirable to make all of these equivalent
solutions have comparable efficiency so that programmers always discover
the most efficient solution.

Source level optimization increases the number of efficient ways to solve a
problem.  This effect is much larger than the increase in the efficiency of
the "best" solution.  Source level optimization transforms the original
program into a more efficient (but equivalent) program.  Although the
optimizer isn't doing anything more than the programmer could have done,
this high-level optimization is important because:

  * The programmer can code simply and directly, rather than changing code
    to please the compiler.

  * When presented with a choice of similar coding alternatives, the
    programmer can chose whichever happens to be most convenient, instead of
    worrying about which is most efficient.

Source level optimization eliminates the need for macros to optimize their
expansion, and also increases the effectiveness of inline expansion.
See sections \ref{source-optimization} and \ref{inline-expansion}.

Efficient support for a safer programming style is the biggest advantage of
source level optimization.  Existing tuned programs will typically perform
roughly the same after source optimization, since their source has already
been optimized by hand.  However, even tuned programs tend to benefit
because:

  * Low level optimization and register allocation provides modest speedups
    in any program.

  * Block compilation and inline expansion can reduce function call
    overhead, but may require some program restructuring.  See sections
    \ref{inline-expansion}, \ref{local-call} and \ref{block-compilation}.

  * Efficiency notes will point out important type declarations that are
    often missed even in highly tuned programs.  \xlref{efficiency-notes}.

  * Existing programs can be compiled safely without prohibitive speed
    penalty, although they would be faster and safer with added
    declarations.  \xlref{type-check-optimization}.

  * The context declaration mechanism allows both space and runtime of large
    systems to be reduced without sacrificing robustness by
    semi-automatically varying compilation policy without addition any
    \code{optimize} declarations to the source.
    \xlref{context-declarations}.

  * Byte compilation can be used to dramatically reduce the size of code
    that can afford to run slower. \xlref{byte-compile}
]#

#[ Function Call

The sort of symbolic programs generally written in Lisp often favor
recursion over iteration, or have inner loops so complex that they involve
multiple function calls.  Such programs spend a larger fraction of their
time doing function calls than is the norm in other languages; for this
reason Lisp implementations strive to make the general (or full) function
call as cheap as possible.  The compiler provides two good alternatives to
full call:

  * Local call resolves function references at compile time, allowing better
    calling sequences and optimization across function calls.
    \xlref{local-call}.

  * Inline expansion totally eliminates call overhead and allows many
    context dependent optimizations.  This provides a safe and efficient
    implementation of operations with function semantics, eliminating the
    need for error-prone macro definitions or manual case analysis.  See
    sections \ref{source-optimization} and \ref{inline-expansion}.

Generally, the compiler provides simple implementations for simple uses of
function call, rather than having only a single calling convention.  These
features allow a more natural programming style:

  * Proper tail recursion.  \xlref{tail-recursion}

  * Relatively efficient closures.

  * A `funcall' that is as efficient as normal named call.

  * Calls to local functions such as from `labels' are optimized:

      * Control transfer is a direct jump.

      * The closure environment is passed in registers rather than heap
	allocated.

      * Keyword arguments and multiple values are implemented more
	efficiently. FIX than?
    \xlref{local-call}.
]#

#[ Representation of Objects

Sometimes traditional Lisp implementation techniques compare so poorly to
the techniques used in other languages that Lisp can become a poor choice
of language.  Very slow number-crunching programs can appear, since Lisp
numeric operations often involve number-consing and generic arithmetic.
The compiler supports efficient natural representations for numbers (and
some other types), and allows these efficient representations to be used in
more contexts.  The compiler also provides good efficiency notes that warn
when a crucial declaration is missing.

The section \ref{non-descriptor} details further object representations and
numeric types.  Also \pxlref{efficiency-notes} about efficiency notes.
]#

#[ Writing Efficient Code

Writing efficient code that works is a complex and prolonged process.  It
is important to keep sight of what the original problem demands.  Remember
that:

  * The program must be correct.

  * Both the programmer and the user may make errors, so the program must be
    robust -- it must detect errors in a way that allows easy correction.

  * A small portion of the program will consume most of the resources, with
    the bulk of the code being having very little effect on efficiency.
    Even experienced programmers familiar with the problem area will find it
    hard to reliably predict where these "hot spots" will be.

The best way to get efficient code that is still worth using, is to separate
coding from tuning.  During coding:

  * Use a coding style that aids correctness and robustness without being
    incompatible with efficiency.

  * Choose appropriate data structures that allow efficient algorithms and
    object representations (\pxlref{object-representation}).  Try to make
    interfaces abstract enough so that you can change to a different
    representation if profiling reveals a need.

  * Whenever you make an assumption about a function argument or global data
    structure, add consistency assertions, either with type declarations or
    explicit uses of `assert', `ecase', etc.

During tuning:

  * Identify the hot spots in the program through [FIX profiling].

  * Use efficiency notes, more profiling, or manual inspection of the source
    to identify constructs that can be improved in the hot spot with.  See
    sections \ref{general-efficiency} and \ref{efficiency-notes}.

  * Add declarations and consider the application of optimizations.  See
    sections \ref{local-call}, \ref{inline-expansion} and
    \ref{non-descriptor}.

  * If all else fails, consider algorithm or data structure changes.  If you
    did a good job coding, changes should be easy to introduce.
]#

#[ More About Types in the Compiler
\cpsubindex{types}{in the compiler}

This section describes in more detail the types and declarations recognized
by the compiler.  Notable features of the type support in the compiler are:

  * Precise type checking that helps to find errors at run time.

  * Compile-time type checking helps to find errors at compile time.

  * Type inference that minimizes the need for generic operations, and also
    increases the efficiency of run time type checking and the effectiveness
    of compile time type checking.

  * Support for detailed types that provides a wealth of opportunity for
    operation-specific type inference and optimization.

[ More Types Meaningful       ]
[ Canonicalization            ]
[ Member Types                ]
[ Union Types                 ]
[ The Empty Type              ]
[ Function Types              ]
[ The Values Declaration      ]
[ Structure Types             ]
[ The Freeze-Type Declaration ]
[ Type Restrictions           ]
[ Type Style Recommendations  ]
]#

#[ More Types Meaningful

A very minimal criterion for type system support is that it be no worse to
make a specific declaration than to make a general one.  The compiler goes
beyond this, taking advantage of the detailed type information.

Using more restrictive types in declarations allows the compiler to do
better type inference and more compile-time type checking.  Also, when type
declarations are considered to be consistency assertions that should be
verified (conditional on policy), then complex types are useful for making
more detailed assertions.

The compiler "understands" the list-style \code{or}, \code{member},
\code{function}, array and number type specifiers.  Understanding means
that:

  * If the type contains more information than is used in a particular
    context, then the extra information is simply ignored, rather than
    derailing type inference.

  * In many contexts, the extra information from these type specifiers is
    used to good effect.  In particular, type checking in the compiler is
    precise, so these complex types can be used in declarations to make
    interesting assertions about functions and data structures
    (\pxlref{precise-type-checks}.)  More specific declarations also aid
    type inference and reduce the cost for type checking.

For related information, \pxlref{numeric-types} for numeric types, and
section \ref{array-types} for array types.
]#

#[ Canonicalization
\cpsubindex{types}{equivalence}
\cindex{canonicalization of types}
\cindex{equivalence of types}

When given a type specifier, the compiler will often rewrite it into an
equivalent type.  This is the mechanism that the compiler uses for
detecting type equivalence.  For example, in the compiler's canonical
representation, these types are equivalent:

    (or list (member :end))
    (or cons (member nil :end))

This has two implications for the user:

  * The standard symbol type specifiers for \code{atom}, \code{null},
    \code{fixnum}, etc., are available.  The \tindexed{null} type is
    actually defined to be (member nil), \tindexed{list} is (or cons null),
    and \tindexed{fixnum} is (signed-byte 30).

  * When the compiler prints out a type, it have changed from the type
    specifier that originally appeared in the program.  This is generally
    OK, but must be taken into consideration when reading compiler error
    messages.
]#

#[ Member Types
\cindex{member types}

The \tindexed{member} type specifier can be used to represent "symbolic"
values, analogous to the enumerated types of Pascal or C.  For example, the
second value of `find-symbol' has this type:

    (member :internal :external :inherited nil)

Member types are very useful for expressing consistency constraints on data
structures, for example:

    (defstruct ice-cream
      (flavor :vanilla :type (member :vanilla :chocolate :strawberry)))

Member types are also useful in type inference, as the number of members
can sometimes be pared down to one, in which case the value is a known
constant.
]#

#[ Union Types
\cindex{union (\code{or}) types}
\cindex{or (union) types}

The \tindexed{or} (union) type specifier is understood, and is
meaningfully applied in many contexts.  The use of \code{or} allows
assertions to be made about types in dynamically typed programs.  For
example:
    (defstruct box
      (next nil :type (or box null))
      (top :removed :type (or box-top (member :removed))))

The type assertion on the \code{top} slot ensures that an error will be signalled
when there is an attempt to store an illegal value (such as \code{:rmoved}.)
Although somewhat weak, these union type assertions provide a useful input into
type inference, allowing the cost of type checking to be reduced.  For example,
this loop is safely compiled with no type checks:
    (defun find-box-with-top (box)
      (declare (type (or box null) box))
      (do ((current box (box-next current)))
          ((null current))
        (fi (eq (box-top current) :removed)
            (return current))))


Union types are also useful in type inference for representing types that are
partially constrained.  For example, the result of this expression:

    (if foo
        (logior x y)
        (list x y))

can be expressed as (or integer cons).
]#

#[ The Empty Type
\cindex{NIL type}
\cpsubindex{empty type}{the}
\cpsubindex{errors}{result type of}

The type nil is also called the empty type, since no object is of type nil.
The union of no types, (or), is also empty.  The compiler's interpretation
of an expression whose type is nil is that the expression never yields any
value, but rather fails to terminate, or is thrown out of.  For example,
the type of a call to `error' or a use of `return' is nil.  When the type
of an expression is empty, compile-time type warnings about its value are
suppressed; presumably somebody else is signalling an error.  If a function
is declared to have return type nil, but does in fact return, then (in safe
compilation policies) a "NIL Function returned" error will be signalled.
See also the function `required-argument'.
]#

#[ Function Types
\cpsubindex{function}{types}
\cpsubindex{types}{function}

\findexed{function} types are understood in the restrictive sense,
specifying:

  * The argument syntax that the function must be called with.  This is
    information about what argument counts are acceptable, and which keyword
    arguments are recognized.  Warnings about argument syntax are a
    consequence of function type checking.

  * The types of the argument values that the caller must pass.  If the
    compiler can prove that some argument to a call is of a type disallowed
    by the called function's type, then it will give a compile-time type
    warning.  In addition to being used for compile-time type checking,
    these type assertions are also used as output type assertions in code
    generation.  For example, if \code{foo} is declared to have a
    \code{fixnum} argument, then the \code{1+} in \code{(foo (1+ x))} is
    compiled with knowledge that the result must be a fixnum.

  * The types the values that will be bound to argument variables in the
    function's definition.  Declaring a function's type with \code{ftype}
    implicitly declares the types of the arguments in the definition.  The
    compiler checks for consistency between the definition and the
    \code{ftype} declaration.  Because of precise type checking, an error
    will be signalled when a function is called with an argument of the
    wrong type.

  * The type of return value(s) that the caller can expect.  This
    information is a useful input to type inference.  For example, if a
    function is declared to return a \code{fixnum}, then when a call to that
    function appears in an expression, the expression will be compiled with
    knowledge that the call will return a \code{fixnum}.

  * The type of return value(s) that the definition must return.  The result
    type in an \code{ftype} declaration is treated like an implicit
    \code{the} wrapped around the body of the definition.  If the definition
    returns a value of the wrong type, an error will be signalled.  If the
    compiler can prove that the function returns the wrong type, then it
    will give a compile-time warning.

Note also, that either the type of a function is declared using a global
\code{ftype} declaration, or the compiler will compute a function type from
the definition, providing a degree of inter-routine type inference,
\pxlref{function-type-inference}.
]#

#[ The Values Declaration
\cindex{values declaration}

Nightshade Lisp include a \code{values} declaration.  The syntax is
\code{(values \i{type1} \i{type2} ... \var{typen}).  This declaration is
semantically equivalent to a `the' form wrapped around the body of the
special form in which the \code{values} declaration appears.  The advantage
of \code{values} over `the' is purely syntactic -- `the' introduces more
indentation.  For example:

    (defun foo (x)
      (declare (values single-float))
      (ecase x
        (:this ...)
        (:that ...)
        (:the-other ...)))

is equivalent to:

   (defun foo (x)
     (the single-float
          (ecase x
            (:this ...)
            (:that ...)
            (:the-other ...))))

and

    (defun floor (number &optional (divisor 1))
      (declare (values integer real))
      ...)

is equivalent to:

    (defun floor (number &optional (divisor 1))
      (the (values integer real)
           ...))

In addition to being recognized by `lambda' (and hence by `defun'), the
\code{values} declaration is recognized by all the other special forms with
bodies and declarations: `let', `let*', `labels' and `flet'.  Macros with
declarations usually splice the declarations into one of the above forms,
so they will accept this declaration too, but the exact effect of a
\code{values} declaration will depend on the macro.

If you declare the types of all arguments to a function, and also declare
the return value types with \code{values}, you have described the type of
the function.  The compiler will use this argument and result type
information to derive a function type that will then be applied to calls of
the function (\pxlref{function-types}.)  This provides a way to declare the
types of functions that is much less syntactically awkward than using the
\code{ftype} declaration with a \code{function} type specifier.

]#

#[ Structure Types
\cindex{structure types}
\cindex{defstruct types}
\cpsubindex{types}{structure}

Because of precise type checking, structure types are well supported by the
compiler:

  * The structure argument to structure accessors is precisely checked -- if
    you call \code{foo-a} on a \code{bar}, an error will be signalled.

  * The types of slot values are precisely checked -- if you pass the wrong
    type argument to a constructor or a slot setter, then an error will be
    signalled.

This error checking is tremendously useful for detecting errors in programs
that manipulate complex data structures.

An additional advantage of checking structure types and enforcing slot
types is that the compiler can safely believe slot type declarations.  the
compiler effectively moves the type checking from the slot access to the
slot setter or constructor call.  This is more efficient since caller of
the setter or constructor often knows the type of the value, entirely
eliminating the need to check the value's type.  Consider this example:
    (defstruct coordinate
     (x nil :type single-float)
     (y nil :type single-float))

    (defun make-it ()
      (make-coordinate :x 1.0 :y 1.0))

    (defun use-it (it)
      (declare (type coordinate it))
      (sqrt (expt (coordinate-x it) 2) (expt (coordinate-y it) 2)))

\code{make-it} and \code{use-it} are compiled with no checking on the types
of the float slots, yet \code{use-it} can use \code{single-float}
arithmetic with perfect safety.  Note that \code{make-coordinate} must
still check the values of \code{x} and \code{y} unless the call is block
compiled or inline expanded (\pxlref{local-call}.)  But even without this
advantage, it is almost always more efficient to check slot values on
structure initialization, since slots are usually written once and read
many times.
]#

#[ The Freeze-Type Declaration
\cindex{freeze-type declaration}

The extensions:freeze-type declaration enables more efficient compilation
of user-defined types by asserting that the definition is to remain
constant.  This declaration may only be used globally (with declaim or
proclaim).  Currently freeze-type only affects structure type testing done
by typep, typecase, etc.  Here is an example:

    (declaim (freeze-type foo bar))

This asserts that the types foo and bar and their subtypes are not going to
change.  This allows more efficient type testing, since the compiler can
open-code a test for all possible subtypes, rather than having to examine
the type hierarchy at run-time.
]#

#[ Type Restrictions
\cpsubindex{types}{restrictions on}

Avoid use of the \code{and}, \code{not} and \code{satisfies} types in
declarations, since type inference has problems with them.  When these
types do appear in a declaration, they are still checked precisely, but the
type information is of limited use to the compiler.  \code{and} types are
effective as long as the intersection can be canonicalized to a type that
doesn't use \code{and}.  For example:
    (and fixnum unsigned-byte)

is fine, since it is the same as:

    (integer 0 \var{most-positive-fixnum})

but this type:

    (and symbol (not (member :end)))

will not be fully understood by type interference since the \code{and}
can't be removed by canonicalization.

Using any of these type specifiers in a type test with `typep' or
`typecase' is fine, since as tests, these types can be translated into the
\code{and} macro, the \code{not} function or a call to the satisfies
predicate.
]#

#[ Type Style Recommendations
\cindex{style recommendations}

The compiler provides good support for some currently strange ways of using
the type system.  It is desirable to make declarations as precise as
possible, but type inference also makes some declarations redundant.  Here
are some general guidelines for maximum robustness and efficiency:

  * Declare the types of all function arguments and structure slots as
    precisely as possible (while avoiding \code{not}, \code{and} and
    \code{satisfies}).  Put these declarations in during initial coding so
    that type assertions can show up coding errors.

  * Use the \tindexed{member} type specifier where there are a small number
    of possible symbol values, for example: \code{(member :red :blue
    :green).

  * Use the \tindexed{or} type specifier in situations where the type is not
    certain, but there are only a few possibilities, for example:
    \code{(or list vector)}.

  * Declare integer types with the tightest bounds possible, such as
    \code{(integer 3 7)}.

  * Define `deftype' or `defstruct' types before they are used.  Definition
    after use is legal, but type tests and structure operations will be
    compiled much less efficiently.

  * Use the \code{extensions:freeze-type} declaration to speed up type
    testing for structure types which won't have new subtypes added later.
    \xlref{freeze-type}

  * In addition to declaring the array element type and simpleness, also
    declare the dimensions if they are fixed, for example:

	(simple-array single-float (1024 1024))

    This bounds information allows array indexing for multi-dimensional
    arrays to be compiled much more efficiently, and may also allow array
    bounds checking to be done at compile time.  \xlref{array-types}.

  * Avoid use of the `the' declaration within expressions.  It clutters the
    code, and has very little effect under safe policies.  If the need for
    an output type assertion is revealed by efficiency notes during tuning,
    then consider `the', but it is preferable to constrain the argument
    types more, allowing the compiler to prove the desired result type.

  * Don't bother declaring the type of `let' or other non-argument variables
    unless the type is non-obvious.  If you declare function return types
    and structure slot types, then the type of a variable is often obvious
    both to the programmer and to the compiler.  An important case where the
    type is ambiguous, and a declaration is appropriate, is when the value
    for a variable is pulled out of an untyped structure (e.g., the result
    of `car'), or comes from some weakly typed function, such as `read'.

  * Declarations are sometimes necessary for integer loop variables, since
    the compiler can't always prove that the value is of a good integer
    type. These declarations are best added during tuning, when an
    efficiency note indicates the need.
]#

#[ Type Inference
\cindex{type inference}
\cindex{inference of types}
\cindex{derivation of types}

Type inference is the process by which the compiler tries to figure out the
types of expressions and variables, given an inevitable lack of complete
type information.  The more precise and comprehensive type declarations
are, the more type inference the compiler will be able to do.

[ Variable Type Inference           ]
[ Local Function Type Inference     ]
[ Global Function Type Inference    ]
[ Operation Specific Type Inference ]
[ Dynamic Type Inference            ]
[ Type Check Optimization           ]
]#

#[ Variable Type Inference

The type of a variable is the union of the types of all the definitions.
In the degenerate case of a let, the type of the variable is the type of
the initial value.  This inferred type is intersected with any declared
type, and is then propagated to all the variable's references.  The types
of `multiple-value-bind' variables are similarly inferred from the types of
the individual values of the values form.

If multiple type declarations apply to a single variable, then all the
declarations must be correct; it is as though all the types were
intersected producing a single \tindexed{and} type specifier.  In this
example:

    (defmacro my-dotimes ((var count) &body body)
      `(do ((,var 0 (1+ ,var)))
           ((>= ,var ,count))
         (declare (type (integer 0 *) ,var))
         ,@body))

    (my-dotimes (i ...)
      (declare (fixnum i))
      ...)

the two declarations for \code{i} are intersected, so \code{i} is known to
be a non-negative fixnum.

In practice, this type inference is limited to lets and local functions,
since the compiler can't analyze all the calls to a global function.  But
type inference works well enough on local variables so that it is often
unnecessary to declare the type of local variables.  This is especially
likely when function result types and structure slot types are declared.
The main areas where type inference breaks down are:

  * When the initial value of a variable is a untyped expression, such as
    \code{\w{(car x)}}, and

  * When the type of one of the variable's definitions is a function of the
    variable's current value, as in: \code{(setq x (1+ x))}
]#

#[ Local Function Type Inference
\cpsubindex{local call}{type inference}

The types of arguments to local functions are inferred in the same was as
any other local variable; the type is the union of the argument types
across all the calls to the function, intersected with the declared type.
If there are any assignments to the argument variables, the type of the
assigned value is unioned in as well.

The result type of a local function is computed in a special way that takes
tail recursion (\pxlref{tail-recursion}) into consideration.  The result
type is the union of all possible return values that aren't tail-recursive
calls.  For example, the compiler will infer that the result type of this
function is \code{integer}:

    (defun ! (n res)
      (declare (integer n res))
      (if (zerop n)
          res
          (! (1- n) (* n res))))

Although this is a rather obvious result, it becomes somewhat less trivial
in the presence of mutual tail recursion of multiple functions.  Local
function result type inference interacts with the mechanisms for ensuring
proper tail recursion mentioned in section \ref{local-call-return}.
]#

#[ Global Function Type Inference
\cpsubindex{function}{type inference}

As described in section \ref{function-types}, a global function type
(\tindexed{ftype}) declaration places implicit type assertions on the call
arguments, and also guarantees the type of the return value.  So wherever a
call to a declared function appears, there is no doubt as to the types of
the arguments and return value.  Furthermore, the compiler will infer a
function type from the function's definition if there is no \code{ftype}
declaration.  Any type declarations on the argument variables are used as
the argument types in the derived function type, and the compiler's best
guess for the result type of the function is used as the result type in the
derived function type.

This method of deriving function types from the definition implicitly assumes
that functions won't be redefined at run-time.  Consider this example:

    (defun foo-p (x)
      (let ((res (and (consp x) (eq (car x) 'foo))))
        (format t "It is ~:[not ~;~]foo." res)))

    (defun frob (it)
      (if (foo-p it)
          (setf (cadr it) 'yow!)
          (1+ it)))

Presumably, the programmer really meant to return \code{res} from
\code{foo-p}, but he seems to have forgotten.  When he tries to call do
\code{\w{(frob (list 'foo nil))}}, \code{frob} will flame out when it tries
to add to a \code{cons}.  Realizing his error, he fixes \code{foo-p} and
recompiles it.  But when he retries his test case, he is baffled because
the error is still there.  What happened in this example is that the
compiler proved that the result of \code{foo-p} is \code{null}, and then
proceeded to optimize away the \code{setf} in \code{frob}.

Fortunately, in this example, the error is detected at compile time due to
notes about unreachable code (\pxlref{dead-code-notes}.)  Still, some users
may not want to worry about this sort of problem during incremental
development, so there is a variable to control deriving function types.

{variable:ext:*derive-function-types*}
]#

#[ Operation Specific Type Inference
\cindex{operation specific type inference}
\cindex{arithmetic type inference}
\cpsubindex{numeric}{type inference}

Many of the standard functions have special type inference procedures that
determine the result type as a function of the argument types.  For
example, the result type of `aref' is the array element type.  Here are
some other examples of type inferences:

    (logand x #xFF) \result{} (unsigned-byte 8)

    (+ (the (integer 0 12) x) (the (integer 0 1) y)) \result{} (integer 0 13)

    (ash (the (unsigned-byte 16) x) -8) \result{} (unsigned-byte 8)
]#

#[ Dynamic Type Inference
\cindex{dynamic type inference}
\cindex{conditional type inference}
\cpsubindex{type inference}{dynamic}

The compiler uses flow analysis to infer types in dynamically typed
programs.  For example:

    (ecase x
      (list (length x))
      ...)

Here, the compiler knows the argument to `length' is a list, because the
call to `length' is only done when \code{x} is a list.  The most
significant efficiency effect of inference from assertions is usually in
type check optimization.

Dynamic type inference has two inputs: explicit conditionals and implicit
or explicit type assertions.  Flow analysis propagates these constraints on
variable type to any code that can be executed only after passing though
the constraint.  Explicit type constraints come from `if's where the test
is either a lexical variable or a function of lexical variables and
constants, where the function is either a type predicate, a numeric
comparison or `eq'.

If there is an `eq' (or `eql') test, then the compiler will actually
substitute one argument for the other in the true branch.  For example:

    (when (eq x :yow!) (return x))

becomes:

    (when (eq x :yow!) (return :yow!))


This substitution is done when one argument is a constant, or one argument
has better type information than the other.  This transformation reveals
opportunities for constant folding or type-specific optimizations.  If the
test is against a constant, then the compiler can prove that the variable
is not that constant value in the false branch, or \code{(not (member
:yow!))} in the example above.  This can eliminate redundant tests, for
example:

    (if (eq x nil)
        ...
        (if x a b))

is transformed to this:

    (if (eq x nil)
        ...
        a)


Variables appearing as `if' tests are interpreted as \code{(not (eq
\var{var} nil))} tests.  The compiler also converts `=' into `eql' where
possible.  It is hard to do inference directly on `=' since it does
implicit coercions.

When there is an explicit `<' or `>' test on integer variables, the
compiler makes inferences about the ranges the variables can assume in the
true and false branches.  This is mainly useful when it proves that the
values are small enough in magnitude to allow open-coding of arithmetic
operations.  For example, in many uses of `dotimes' with a \code{fixnum}
repeat count, the compiler proves that fixnum arithmetic can be used.

Implicit type assertions are quite common, especially if you declare
function argument types.  Dynamic inference from implicit type assertions
sometimes helps to disambiguate programs to a useful degree, but is most
noticeable when it detects a dynamic type error.  For example:

    (defun foo (x)
      (+ (car x) x))

results in this warning:

    In: DEFUN FOO
      (+ (CAR X) X)
    ==>
      X
    Warning: Result is a LIST, not a NUMBER.


Note that Lisp's dynamic type checking semantics make dynamic type
inference useful even in programs that are really statically typed, for
example:

    (+ (car x) (length x))


Here, \code{x} presumably always holds a list, but in the absence of a
declaration the compiler cannot assume \code{x} is a list simply because
list-specific operations are sometimes done on it.  The compiler must
consider the program to be dynamically typed until it proves otherwise.
Dynamic type inference proves that the argument to \code{length} is always
a list because the call to `length' is only done after the list-specific
`car' operation.
]#

#[ Type Check Optimization
\cpsubindex{type checking}{optimization}
\cpsubindex{optimization}{type check}

The compiler backs up its support for precise type checking by minimizing
the cost of run-time type checking.  This is done both through type
inference and though optimizations of type checking itself.

Type inference often allows the compiler to prove that a value is of the
correct type, and thus no type check is necessary.  For example:

    (defstruct foo a b c)
    (defstruct link
      (foo (required-argument) :type foo)
      (next nil :type (or link null)))

    (foo-a (link-foo x))

Here, the result of \code{link-foo} is always a \code{foo}, so the check
can be left out.  Even when some type checks are necessary, type inference
can often reduce the number:

    (defun test (x)
      (let ((a (foo-a x))
            (b (foo-b x))
            (c (foo-c x)))
        ...))

In this example, only one \code{(foo-p x)} check is needed.  This applies
to a lesser degree in list operations, such as:

    (if (eql (car x) 3) (cdr x) y)

Here, we only have to check that \code{x} is a list once.

Since the compiler recognizes explicit type tests, code that explicitly
protects itself against type errors has little introduced overhead due to
implicit type checking.  For example, this loop compiles with no implicit
checks checks for `car' and `cdr':

    (defun memq (e l)
      (do ((current l (cdr current)))
          ((atom current) nil)
        (when (eq (car current) e) (return current))))


\cindex{complemented type checks}

The compiler reduces the cost of checks that must be done through an
optimization called \var{complementing}.  A complemented check for
\var{type} is simply a check that the value is not of the type \code{(not
\var{type})}.  This is only interesting when something is known about the
actual type, in which case we can test for the complement of \code{(and
\var{known-type} (not \var{type}))}, or the difference between the known
type and the assertion.  An example:

    (link-foo (link-next x))

Here, we change the type check for \code{link-foo} from a test for
\code{foo} to a test for:

    (not (and (or foo null) (not foo)))

or more simply \code{(not null)}.  This is probably the most important use
of complementing, since the situation is fairly common, and a \code{null}
test is much cheaper than a structure type test.

Here is a more complicated example that illustrates the combination of
complementing with dynamic type inference:

    (defun find-a (a x)
      (declare (type (or link null) x))
      (do ((current x (link-next current)))
          ((null current) nil)
        (let ((foo (link-foo current)))
          (when (eq (foo-a foo) a) (return foo)))))

This loop can be compiled with no type checks.  The \code{link} test for
\code{link-foo} and \code{link-next} is complemented to \w{\code{(not
null)}}, and then deleted because of the explicit \code{null} test.  As
before, no check is necessary for \code{foo-a}, since the \code{link-foo}
is always a \code{foo}.  This sort of situation shows how precise type
checking combined with precise declarations can actually result in reduced
type checking.
]#

#[ Source Optimization
\cindex{optimization}

This section describes source-level transformations that the compiler does
on programs in an attempt to make them more efficient.  Although
source-level optimizations can make existing programs more efficient, the
biggest advantage of this sort of optimization is that it makes it easier
to write efficient programs.  If a clean, straightforward implementation is
can be transformed into an efficient one, then there is no need for tricky
and dangerous hand optimization.

[ Let Optimization                ]
[ Constant Folding                ]
[ Unused Expression Elimination   ]
[ Control Optimization            ]
[ Unreachable Code Deletion       ]
[ Multiple Values Optimization    ]
[ Source to Source Transformation ]
[ Style Recommendations           ]
]#

#[ Let Optimization

\cindex{let optimization} \cpsubindex{optimization}{let}
The primary optimization of let variables is to delete them when they are
unnecessary.  Whenever the value of a let variable is a constant, a
constant variable or a constant (local or non-notinline FIX?) function, the
variable is deleted, and references to the variable are replaced with
references to the constant expression.  This is useful primarily in the
expansion of macros or inline functions, where argument values are often
constant in any given call, but are in general non-constant (FIX variable?)
expressions that must be bound to preserve order of evaluation.  Let
variable optimization eliminates the need for macros to carefully avoid
spurious bindings, and also makes inline functions just as efficient as
macros.

A particularly interesting class of constant is a local function.  Substituting
for lexical variables that are bound to a function can substantially improve
the efficiency of functional programming styles, for example:
    (let ((a #'(lambda (x) (zow x))))
      (funcall a 3))

effectively transforms to:

    (zow 3)

This transformation is done even when the function is a closure, as in:

    (let ((a (let ((y (zug)))
               #'(lambda (x) (zow x y)))))
      (funcall a 3))

becoming:

    (zow 3 (zug))


A constant variable is a lexical variable that is never assigned to, always
keeping its initial value.  Whenever possible, avoid setting lexical
variables -- instead bind a new variable to the new value.  Except for loop
variables, it is almost always possible to avoid setting lexical variables.
This form:

    (let ((x (f x)))
      ...)

is \var{more} efficient than this form:

    (setq x (f x))
    ...


Setting variables makes the program more difficult to understand, both to
the compiler and to the programmer.  Most let optimizations are only done
on constant variables.

Constant variables with only a single use are also optimized away, even
when the initial value is not constant.  (The source transformation in this
example doesn't represent the preservation of evaluation order implicit in
the compiler's internal representation.  Where necessary, the back end will
reintroduce temporaries to preserve the semantics.)  For example, this
expansion of `incf':

    (let ((#:g3 (+ x 1)))
      (setq x #:G3))

becomes:

    (setq x (+ x 1))

The type semantics of this transformation are more important than the
elimination of the variable itself.  Consider what happens when \code{x} is
declared to be a \code{fixnum}; after the transformation, the compiler can
compile the addition knowing that the result is a \code{fixnum}, whereas
before the transformation the addition would have to allow for fixnum
overflow.

Another variable optimization deletes any variable that is never read.  This
causes the initial value and any assigned values to be unused, allowing those
expressions to be deleted if they have no side-effects.

Note that a let is actually a degenerate case of local call
(\pxlref{let-calls}), and that let optimization can be done on calls that
weren't created by a let.  Also, local call allows an applicative style of
iteration that is totally assignment free.
]#

#[ Constant Folding
\cindex{constant folding}
\cpsubindex{folding}{constant}

Constant folding is an optimization that replaces a call of constant
arguments with the constant result of that call.  Constant folding is done
on all standard functions for which it is legal.  Inline expansion allows
folding of any constant parts of the definition, and can be done even on
functions that have side-effects.

It is convenient to rely on constant folding when programming, as in this
example:

    (defconstant limit 42)

    (defun foo ()
      (... (1- limit) ...))

Constant folding is also helpful when writing macros or inline functions,
since it usually eliminates the need to write a macro that special-cases
constant arguments.

\cindex{constant-function declaration}

Constant folding of a user defined function is enabled by the
\code{extensions:constant-function} proclamation.   In this example:

    (declaim (ext:constant-function myfun))
    (defun myexp (x y)
      (declare (single-float x y))
      (exp (* (log x) y)))

     ... (myexp 3.0 1.3) ...

The call to \code{myexp} is constant-folded to \code{4.1711674}.
]#

#[ Unused Expression Elimination
\cindex{unused expression elimination}
\cindex{dead code elimination}

If the value of any expression is not used, and the expression has no
side-effects, then it is deleted.  As with constant folding, this
optimization applies most often when cleaning up after inline expansion and
other optimizations.  Any function declared an
\code{extensions:constant-function} is also subject to unused expression
elimination.

Note that the compiler will eliminate parts of unused expressions known to
be side-effect free, even if there are other unknown parts.  For example:

    (let ((a (list (foo) (bar))))
      (if t
          (zow)
          (raz a)))

becomes:

    (progn (foo) (bar))
    (zow)
]#

#[ Control Optimization
\cindex{control optimization}
\cpsubindex{optimization}{control}

The most important optimization of control is recognizing when an `if' test
is known at compile time, then deleting the `if', the test expression, and
the unreachable branch of the `if'.  This can be considered a special case
of constant folding, although the test doesn't have to be truly constant as
long as it is definitely true.  Note also, that type inference propagates
the result of an `if' test to the true and false branches,
\pxlref{constraint-propagation}.

A related `if' optimization is this transformation (note that the code
for \code{x} and \code{y} isn't actually replicated):

    (if (if a b c) x y)

into:

    (if a
        (if b x y)
        (if c x y))

The opportunity for this sort of optimization usually results from a
conditional macro.  For example:

    (if (not a) x y)

is actually implemented as this:

    (if (if a nil t) x y)

which is transformed to this:

    (if a
        (if nil x y)
        (if t x y))

which is then optimized to this:

    (if a y x)

Note that due to the compiler's internal representations, the `if'-`if'
situation will be recognized even if other forms are wrapped around the
inner `if', like:

    (if (let ((g ...))
          (loop
            ...
            (return (not g))
            ...))
        x y)


In the compiler, all the control macros really are macros, written in terms
of `if', `block' and `tagbody', so user-defined control macros can be just
as efficient as the standard ones.  The compiler emits basic blocks using a
heuristic that minimizes the number of unconditional branches.  The code in
a `tagbody' will not be emitted in the order it appeared in the source, so
there is no point in arranging the code to make control drop through to the
target.
]#

#[ Unreachable Code Deletion
\cindex{unreachable code deletion}
\cindex{dead code elimination}

The compiler will delete code whenever it can prove that the code can never
be executed.  Code becomes unreachable when:

  * An `if' is optimized away, or

  * There is an explicit unconditional control transfer such as `go' or
    `return-from', or

  * The last reference to a local function is deleted (or there never was
    any reference.)

When code that appeared in the original source is deleted, the compiler
prints a note to indicate a possible problem (or at least unnecessary
code).  For example:

    (defun foo ()
      (if t
          (write-line "True.")
          (write-line "False.")))

will result in this note:

    In: DEFUN FOO
      (WRITE-LINE "False.")
    Note: Deleting unreachable code.


It is important to pay attention to unreachable code notes, since they
often indicate a subtle type error.  For example:

    (defstruct foo a b)

    (defun lose (x)
      (let ((a (foo-a x))
            (b (if x (foo-b x) :none)))
        ...))

results in this note:

    In: DEFUN LOSE
      (IF X (FOO-B X) :NONE)
    ==>
      :NONE
    Note: Deleting unreachable code.

The \kwd{none} is unreachable, because type inference knows that the
argument to \code{foo-a} must be a \code{foo}, and thus must be true.
Presumably the programmer forgot that \code{x} could be () when he wrote
the binding for \code{a}.

Here is an example with an incorrect declaration:

    (defun count-a (string)
      (do ((pos 0 (position #\back a string :start (1+ pos)))
           (count 0 (1+ count)))
          ((null pos) count)
        (declare (fixnum pos))))

This time our note is:

    In: DEFUN COUNT-A
      (DO ((POS 0 #) (COUNT 0 #))
          ((NULL POS) COUNT)
        (DECLARE (FIXNUM POS)))
    --> BLOCK LET TAGBODY RETURN-FROM PROGN
    ==>
      COUNT
    Note: Deleting unreachable code.

The problem here is that \code{pos} is always true, since it is declared a
\code{fixnum}.

It takes some experience with unreachable code notes to be able to tell what
they are trying to say.  In non-obvious cases, the best thing to do is to call
the function in a way that should cause the unreachable code to be executed.
Either you will get a type error, or you will find that there truly is no way
for the code to be executed.

Not all unreachable code results in a note:

  * A note is only given when the unreachable code textually appears in the
    original source.  This prevents spurious notes due to the optimization
    of macros and inline functions, but sometimes also foregoes a note that
    would have been useful.

  * Since accurate source information is not available for non-list forms,
    there is an element of heuristic in determining whether or not to give a
    note about an atom.  Spurious notes may be given when a macro or inline
    function defines a variable that is also present in the calling
    function. Notes about () and #t are never given, since it is too easy to
    confuse these constants in expanded code with ones in the original
    source.

  * Notes are only given about code unreachable due to control flow.  There
    is no note when an expression is deleted because its value is unused,
    since this is a common consequence of other optimizations.

Somewhat spurious unreachable code notes can also result when a macro inserts
multiple copies of its arguments in different contexts, for example:
    (defmacro t-and-f (var form)
      `(if ,var ,form ,form))

    (defun foo (x)
      (t-and-f x (if x "True." "False.")))

results in these notes:

    In: DEFUN FOO
      (IF X "True." "False.")
    ==>
      "False."
    Note: Deleting unreachable code.

    ==>
      "True."
    Note: Deleting unreachable code.

It seems like it has deleted both branches of the `if', but it has really
deleted one branch in one copy, and the other branch in the other copy.
Note that these messages are only spurious in not satisfying the intent of
the rule that notes are only given when the deleted code appears in the
original source; there is always some code being deleted when a unreachable
code note is printed.
]#

#[ Multiple Values Optimization
\cindex{multiple value optimization}
\cpsubindex{optimization}{multiple value}

Within a function, the compiler implements uses of multiple values
particularly efficiently.  Multiple values can be kept in arbitrary
registers, so using multiple values doesn't imply stack manipulation and
representation conversion.  For example, this code:
    (let ((a (if x (foo x) u))
          (b (if x (bar x) v)))
      ...)

is actually more efficient written this way:

    (multiple-value-bind
        (a b)
        (if x
            (values (foo x) (bar x))
            (values u v))
      ...)


Also, \pxlref{local-call-return} for information on how local call provides
efficient support for multiple function return values.
]#

#[ Source to Source Transformation
\cindex{source-to-source transformation}
\cpsubindex{transformation}{source-to-source}

The compiler implements a number of operation-specific optimizations as
source-to-source transformations.  You will often see unfamiliar code in
error messages, for example:
    (defun my-zerop () (zerop x))

gives this warning:

    In: DEFUN MY-ZEROP
      (ZEROP X)
    ==>
      (= X 0)
    Warning: Undefined variable: X

The original `zerop' has been transformed into a call to `='.  This
transformation is indicated with the same ==> used to mark macro and
function inline expansion.  Although it can be confusing, display of the
transformed source is important, since warnings are given with respect to
the transformed source.  This a more obscure example:

    (defun foo (x) (logand 1 x))

gives this efficiency note:

    In: DEFUN FOO
      (LOGAND 1 X)
    ==>
      (LOGAND C ]Y C ]X)
    Note: Forced to do static-function Two-arg-and (cost 53).
          Unable to do inline fixnum arithmetic (cost 1) because:
          The first argument is a INTEGER, not a FIXNUM.
          etc.

Here, the compiler commuted the call to `logand', introducing temporaries.
The note complains that the first argument is not a \code{fixnum}, when in
the original call, it was the second argument.  To make things more
confusing, the compiler introduced temporaries called \code{c ]x} and
\code{c ]y} that are bound to \code{y} and \code{1}, respectively.

You will also notice source-to-source optimizations when efficiency notes
are enabled (\pxlref{efficiency-notes}).  When the compiler is unable to do
a transformation that might be possible if there was more information, then
an efficiency note is printed.  For example, \code{my-zerop} above will
also give this efficiency note:
    In: DEFUN FOO
      (ZEROP X)
    ==>
      (= X 0)
    Note: Unable to optimize because:
          Operands might not be the same type, so can't open code.
]#

#[ Style Recommendations
\cindex{style recommendations}

Source level optimization makes possible a clearer and more relaxed
programming style:

  * Use inline functions instead of macros to save a function call.  Inline
    functions are functions declare it inline.  They are clearer, less
    error-prone, and work just as well.

  * Let the compiler do the trivial optimizations of macro expansions, such
    as working around binding variables for simple expressions.  The
    compiler is less likely to make a mistake.

  * Make use of local functions (i.e., `labels' or `flet') and
    tail-recursion in places where it is clearer.  Local function call is
    faster than full call.

  * Stay away from setting local variables when possible.  Binding a new
    `let' variable is at least as efficient as setting an existing variable,
    and is easier to understand, both for the compiler and the programmer.

  * Instead of writing similar code over and over again so that it can be
    hand customized for each use, define a macro or inline function, and let
    the compiler do the work.
]#

#[ Tail Recursion
\cindex{tail recursion}
\cindex{recursion}

A recursive call is tail-recursive if nothing has to be done after the call
returns, i.e. when the call returns, the returned value is immediately
returned from the calling function.  In this example, the recursive call to
\code{myfun} is tail-recursive:

    (defun myfun (x)
      (if (oddp (random x))
          (isqrt x)
          (myfun (1- x))))


Tail recursion is interesting because it is a form of recursion that can be
implemented much more efficiently than general recursion.  In general, a
recursive call requires the compiler to allocate storage on the stack at
run-time for every call that is made.  This memory consumption makes
recursion too slow for representing repetitive algorithms having large or
open sizes.  Tail recursion is the special case of recursion that is
semantically equivalent to the iteration constructs normally used to
represent repetition in programs.  Because tail recursion is equivalent to
iteration, tail-recursive programs can be compiled as efficiently as
iterative programs.

So why write a program recursively when it can be expressed using a loop?
Well, the main answer is that recursion is a more general mechanism, so it
can express some solutions simply that are awkward to write as a loop.
Some programmers also feel that recursion is a stylistically preferable way
to write loops because it avoids assigning variables.  For example:

    (defun fun1 (x)
      something-that-uses-x)

    (defun fun2 (y)
      something-that-uses-y)

    (do ((x something (fun2 (fun1 x))))
        (nil))

can be written as:

    (defun fun1 (x)
      (fun2 something-that-uses-x))

    (defun fun2 (y)
      (fun1 something-that-uses-y))

    (fun1 something)

The tail-recursive definition is actually more efficient, in addition to
being (arguably) clearer.  As the number of functions and the complexity of
their call graph increases, the simplicity of using recursion becomes
compelling.  Consider the advantages of writing a large finite-state
machine with separate tail-recursive functions instead of using a single
huge `prog'.

It helps to understand how to use tail recursion if you think of a
tail-recursive call as a `psetq' that assigns the argument values to the
called function's variables, followed by a `go' to the start of the called
function.  This makes clear an inherent efficiency advantage of
tail-recursive call: in addition to not having to allocate a stack frame,
there is no need to prepare for the call to return (e.g., by computing a
return PC).

Is there any disadvantage to tail recursion?  Other than an increase in
efficiency, the only way you can tell that a call has been compiled
tail-recursively is if you use the debugger.  Since a tail-recursive call
has no stack frame, there is no way the debugger can print out the stack
frame representing the call.  The effect is that backtrace will not show
some calls that would have been displayed in a non-tail-recursive
implementation.  In practice, this is not as bad as it sounds -- in fact it
isn't really clearly worse, just different.  \xlref{debug-tail-recursion}
for information about the debugger implications of tail recursion.

In order to ensure that tail-recursion is preserved in arbitrarily complex
calling patterns across separately compiled functions, the compiler must
compile any call in a tail-recursive position as a tail-recursive call.
This is done regardless of whether the program actually exhibits any sort
of recursive calling pattern.  In this example, the call to \code{fun2}
will always be compiled as a tail-recursive call:

    (defun fun1 (x)
      (fun2 x))

So tail recursion doesn't necessarily have anything to do with recursion as
it is normally thought of.  \xlref{local-tail-recursion} for more
discussion of using tail recursion to implement loops.

[ Tail Recursion Exceptions ]
]#

#[ Tail Recursion Exceptions

Although the compiler is claimed to be "properly" tail-recursive, some
might dispute this, since there are situations where tail recursion is
inhibited:

  * When the call is enclosed by a special binding, or

  * When the call is enclosed by a `catch' or `unwind-protect, or

  * When the call is enclosed by a `block' or `tagbody' and the block name or
    \code{go} tag has been closed over.

These dynamic extent binding forms inhibit tail recursion because they
allocate stack space to represent the binding.  Shallow-binding
implementations of dynamic scoping also require cleanup code to be
evaluated when the scope is exited.
]#

#[ Local Call
\cindex{local call}
\cpsubindex{call}{local}
\cpsubindex{function call}{local}

The compiler supports two kinds of function call: full call and local call.
Full call is the standard calling convention; its late binding and
generality make Lisp what it is, but create overheads.  When the compiler
can compile the calling function and the called function simultaneously, it
can use local call to save on some of the overhead of full call.  Local
call is really a collection of compilation strategies.  If some aspect of
call overhead is not needed in a particular local call, then it can be
omitted.  In some cases, local call can be totally free.  Local call
provides two main advantages to the user:

  * Local call makes the use of the lexical function binding forms `flet'
    and `labels' much more efficient.  A local call is always faster than a
    full call, and in many cases is much faster.

  * Local call is a natural approach to block compilation, a compilation
    technique that resolves function references at compile time.  Block
    compilation speeds function call, but increases compilation times and
    prevents function redefinition.

[ Self-Recursive Calls ]
[ Let Calls            ]
[ Closures             ]
[ Local Tail Recursion ]
[ Return Values        ]
]#

#[ Self-Recursive Calls
\cpsubindex{recursion}{self}

Local call is used when a function defined by `defun' calls itself.  For
example:

    (defun fact (n)
      (if (zerop n)
          1
          (* n (fact (1- n)))))

This use of local call speeds recursion, but can also complicate debugging,
since `trace' will only show the first call to \code{fact}, and not the
recursive calls.  This is because the recursive calls directly jump to the
start of the function, and don't indirect through the `symbol-function'.
Self-recursive local call is inhibited when the \kwd{block-compile}
argument to `compile-file' is () (\pxlref{compile-file-block}.)
]#

#[ Let Calls

Because local call saves call overheads, the compiler
internally uses local call to implement some macros and special forms that
are not normally thought of as involving a function call.  For example,
this `let':

    (let ((a (foo))
          (b (bar)))
      ...)

is internally represented as though it was macroexpanded into:

    (funcall #'(lambda (a b)
                 ...)
             (foo)
             (bar))

This implementation is acceptable because the simple cases of local call
(equivalent to a `let') result in good code.  This doesn't make `let' any
more efficient, but does make local calls that are semantically the same as
`let' much more efficient than full calls.  For example, these definitions
are all the same as far as the compiler is concerned:

    (defun foo ()
      ...some other stuff...
      (let ((a something))
        ...some stuff...))

    (defun foo ()
      (flet ((localfun (a)
               ...some stuff...))
        ...some other stuff...
        (localfun something)))

    (defun foo ()
      (let ((funvar #'(lambda (a)
                        ...some stuff...)))
        ...some other stuff...
        (funcall funvar something)))

Although local call is most efficient when the function is called only
once, a call doesn't have to be equivalent to a `let' to be more efficient
than full call.  All local calls avoid the overhead of argument count
checking and keyword argument parsing, and there are a number of other
advantages that apply in many common situations.  \xlref{let-optimization}
for a discussion of the optimizations done on let calls.
]#

#[ Closures

\cindex{closures}

Local call allows for much more efficient use of closures, since the closure
environment doesn't need to be allocated on the heap, or even stored in memory
at all.  In this example, there is no penalty for \code{localfun} referencing
\code{a} and \code{b}:
    (defun foo (a b)
      (flet ((localfun (x)
               (1+ (* a b x))))
        (if (= a b)
            (localfun (- x))
            (localfun x))))

In local call, the compiler effectively passes closed-over values as extra
arguments, so there is no need for you to "optimize" local function use by  ; FIX mention somewhere style
explicitly passing in lexically visible values.  Closures may also be
subject to let optimization (\pxlref{let-optimization}.)

Note: indirect value cells are currently always allocated on the heap when
a variable is both assigned to (with `setq' or `setf') and closed over,
regardless of whether the closure is a local function.  This is another
reason to avoid setting variables when you don't have to.
]#

#[ Local Tail Recursion
\cindex{tail recursion}
\cpsubindex{recursion}{tail}

Tail-recursive local calls are particularly efficient, since they are in
effect an assignment plus a control transfer.  Scheme programmers write
loops with tail-recursive local calls, instead of using the imperative `go'
and `setq'.  The compiler provides the option of writing loops such as in
this factorial function:

    (defun ! (n)
      (labels ((loop (n total)
                 (if (zerop n)
                     total
                     (loop (1- n) (* n total)))))
        (loop n 1)))

[FIX] mention this with ~ loop,do changes, loop style info

{macro:ext:iterate}

Here is the factorial example rewritten using `iterate':

    (defun ! (n)
      (iterate loop
               ((n n)
                (total 1))
        (if (zerop n)
            total
            (loop (1- n) (* n total)))))

The main advantage of using `iterate' over `do' is that `iterate' naturally
allows stepping to depend on conditionals in the body of the loop.
`iterate' can also be used to implement algorithms that aren't really
iterative by simply doing a call before the tail of the body.  For example,
the standard recursive definition of factorial can be written like this:

    (iterate fact
             ((n n))
      (if (zerop n)
          1
          (* n (fact (1- n)))))
]#

#[ Return Values
\cpsubindex{return values}{local call}
\cpsubindex{local call}{return values}

One of the more subtle costs of full call comes from allowing arbitrary
numbers of return values.  This overhead can be avoided in local calls to
functions that always return the same number of values.  For efficiency
reasons (as well as stylistic ones), you should write functions so that
they always return the same number of values.  This may require passing
extra () arguments to `values' in some cases, but the result is more
efficient, not less so.

When efficiency notes are enabled (\pxlref{efficiency-notes}), and the
compiler wants to use known values return, but can't prove that the
function always returns the same number of values, then it will print a
note like this:

    In: DEFUN GRUE
      (DEFUN GRUE (X) (DECLARE (FIXNUM X)) (COND (# #) (# NIL) (T #)))
    Note: Return type not fixed values, so can't use known return convention:
      (VALUES (OR (INTEGER -536870912 -1) NULL) &REST T)


In order to implement proper tail recursion in the presence of known values
return (\pxlref{tail-recursion}), the compiler sometimes must prove that
multiple functions all return the same number of values.  When this can't
be proven, the compiler will print a note like this:
    In: DEFUN BLUE
      (DEFUN BLUE (X) (DECLARE (FIXNUM X)) (COND (# #) (# #) (# #) (T #)))
    Note: Return value count mismatch prevents known return from
          these functions:
      BLUE
      SNOO

\xlref{number-local-call} for the interaction between local call and the
representation of numeric types.
]#

#[ Block Compilation
\cindex{block compilation}
\cpsubindex{compilation}{block}

Block compilation allows calls to global functions defined by `defun' to be
compiled as local calls.  The function call can be in a different top-level
form than the `defun', or even in a different file.

In addition, block compilation allows the declaration of the entry points
to the block compiled portion.  An entry point is any function that may be
called from outside of the block compilation.  If a function is not an
entry point, then it can be compiled more efficiently, since all calls are
known at compile time.  In particular, if a function is only called in one
place, then it will be let converted.  This effectively inline expands the
function, but without the code duplication that results from defining the
function normally and then declaring it inline.

The main advantage of block compilation is that it it preserves efficiency in
programs even when (for readability and syntactic convenience) they are broken
up into many small functions.  There is absolutely no overhead for calling a
non-entry point function that is defined purely for modularity (i.e. called
only in one place).

Block compilation also allows the use of non-descriptor arguments and
return values in non-trivial programs (\pxlref{number-local-call}).

[ Block Compilation Semantics    ]
[ Block Compilation Declarations ]
[ Compiler Arguments             ]
[ Practical Challenges           ]
[ Context Declarations           ]
[ Context Declaration Example    ]
]#

#[ Block Compilation Semantics

The effect of block compilation can be envisioned as the compiler turning
all the `defun's in the block compilation into a single `labels' form:

    (declaim (start-block fun1 fun3))

    (defun fun1 ()
      ...)

    (defun fun2 ()
      ...
      (fun1)
      ...)

    (defun fun3 (x)
      (if x
          (fun1)
          (fun2)))

    (declaim (end-block))

becomes:

(labels ((fun1 ()
           ...)
         (fun2 ()
           ...
           (fun1)
           ...)
         (fun3 (x)
           (if x
               (fun1)
               (fun2))))
  (setf (fdefinition 'fun1) #'fun1)
  (setf (fdefinition 'fun3) #'fun3))
Calls between the block compiled functions are local calls, so changing the
global definition of `fun1' will have no effect on what `fun2' does; `fun2'
will keep calling the old `fun1'.

The entry points `fun1' and `fun3' are still installed in the
`symbol-function' as the global definitions of the functions, so a full
call to an entry point works just as before.  However, `fun2' only defined
locally.  In addition, `fun2' is only called in one place, so it will be
let converted.
]#

#[ Block Compilation Declarations
\cpsubindex{declarations}{block compilation}
\cindex{start-block declaration}
\cindex{end-block declaration}

The \code{extensions:start-block} and \code{extensions:end-block}
declarations allow fine-grained control of block compilation.  These
declarations are only legal as a global declarations (`declaim' or
`proclaim').

The \code{start-block} declaration has this syntax:

    (start-block entry-point-name*)

When processed by the compiler, this declaration marks the start of block
compilation, and specifies the entry points to that block.  If no entry
points are specified, then all functions are made into entry points.  If
already block compiling, then the compiler ends the current block and
starts a new one.

The \code{end-block} declaration has no arguments:

    (end-block)

The \code{end-block} declaration ends a block compilation unit without
starting a new one.  This is useful mainly when only a portion of a file is
worth block compiling.
]#

#[ Compiler Arguments
\cpsubindex{compile-file}{block compilation arguments}

The \kwd{block-compile} and \kwd{entry-points} arguments to
`extensions:compile-from-stream' and `compile-file' provide overall control
of block compilation, and allow block compilation without requiring
modification of the program source.

There are three possible values of the \kwd{block-compile} argument:

  % ()

    Do no compile-time resolution of global function names, not even for
    self-recursive calls.  This inhibits any \code{start-block}
    declarations appearing in the file, allowing all functions to be
    incrementally redefined.

  % #t

    Start compiling in block compilation mode.  This is mainly useful for
    block compiling small files that contain no \code{start-block}
    declarations.  See also the \kwd{entry-points} argument.

  % :specified

    Start compiling in form-at-a-time mode, but exploit \code{start-block}
    declarations and compile self-recursive calls as local calls.  Normally
    \kwd{specified} is the default for this argument (see
    *block-compile-default*.)

The \kwd{entry-points} argument can be used in conjunction with
\kwd{block-compile} #t to specify the entry-points to a block-compiled
file.  If not specified or (), all global functions will be compiled as
entry points.  This argument is considered only when \kwd{block-compile} is
true.

{variable:*block-compile-default*}
]#

#[ Practical Challenges

The main problem with block compilation is that the compiler uses large
amounts of memory when it is block compiling.  This places an upper limit
on the amount of code that can be block compiled as a unit.  To make best
use of block compilation, it is necessary to locate the parts of the
program containing many internal calls, and then add the appropriate
\code{start-block} declarations.  When writing new code, it is a good idea
to put in block compilation declarations from the very beginning, since
writing block declarations correctly requires accurate knowledge of the
program's function call structure.  To initially develop code with full
incremental redefinition, compile with *block-compile-default* set to ().

Note if a `defun' appears in a true lexical environment, then calls to it
cannot be block compiled.

;; FIX confirm
Unless files are very small, it is probably impractical to block compile
multiple files as a unit by specifying a list of files to
\code{compile-file}.  Semi-inline expansion (\pxlref{semi-inline}) provides
another way to extend block compilation across file boundaries.
]#

;; FIX duplicated somewhere
#[ Context Declarations

\cindex{context sensitive declarations}
\cpsubindex{declarations}{context-sensitive}

Nightshade has a context-sensitive declaration mechanism which is useful
because it allows flexible control of the compilation policy in large
systems without requiring changes to the source files.  The primary use of
this feature is to allow the exported interfaces of a system to be compiled
more safely than the system internals.  The context used is the name being
defined and the kind of definition (function, macro, etc.)

The \kwd{context-declarations} option to `with-compilation-unit' has
dynamic scope, affecting all compilation done during the evaluation of the
body.  The argument to this option should evaluate to a list of lists of
the form:

    (context-spec declare-form*)

In the indicated context, the specified declare forms are inserted at the
head of each definition.  The declare forms for all contexts that match are
appended together, with earlier declarations getting precedence over later
ones.  A simple example:

    :context-declarations
    '((:external (declare (optimize (safety 2)))))

This will cause all functions that are named by external symbols to be
compiled with \code{safety 2}.

The full syntax of context specs is:

  % :internal :external

    True if the symbol is internal (external) in its home package.

  % :uninterned

    True if the symbol has no home package.

  % :package package-name*

    True if the symbol's home package is in any of the named packages
    (false if uninterned.)

  % :anonymous

    True if the function doesn't have any interesting name (not
    `defmacro', `defun', `labels' or `flet').

  % :macro, :function

    :macro is a global (`defmacro') macro.  :function is anything
    else.

  % :local, :global

    :local is a `labels' or `flet'.  :global is anything else.

  % (:or context-spec*)

    True when any supplied context-spec is true.

  % (:and context-spec*)

    True only when all supplied code{context-spec}s are true.

  % (:not context-spec*)

    True when context-spec is false.

  % (:member name*)

    True when the defined name is one of these names (`equal' test).

  % (:match pattern*)

    True when any of the patterns is a substring of the name.  The name is
    wrapped with $'s, so "$FOO" matches names beginning with "FOO", etc.
]#

#[ Context Declaration Example

Here is a more complex example of `with-compilation-unit' options:

    :optimize '(optimize (speed 2) (space 2) (inhibit-warnings 2)
                         (debug 1) (safety 0))
    :optimize-interface '(optimize-interface (safety 1) (debug 1))
    :context-declarations
    '(((:or :external (:and (:match "%") (:match "SET")))
       (declare (optimize-interface (safety 2))))
      ((:or (:and :external :macro)
            (:match "$PARSE-"))
       (declare (optimize (safety 2)))))

The \code{optimize} and \code{extensions:optimize-interface} declarations
(\pxlref{optimize-declaration}) set up the global compilation policy.  The
bodies of functions are to be compiled completely unsafe (\code{safety 0}),
but argument count and weakened argument type checking is to be done when a
function is called (\code{speed 2 safety 1}).

The first declaration specifies that all functions that are external or
whose names contain both "%" and "SET" are to be compiled with completely
safe interfaces (\code{safety 2}).  The reason for this particular :match
rule is that `setf' inverse functions in this system tend to have both
strings in their name somewhere.  We want `setf' inverses to be safe
because they are implicitly called by users even though their name is not
exported.

The second declaration makes external macros or functions whose names start
with "PARSE-" have safe bodies (as well as interfaces).  This is desirable
because a syntax error in a macro may cause a type error inside the body.
The :match rule is used because macros often have auxiliary functions whose
names begin with this string.

This particular example is used to build part of the standard Nightshade
system.  Note however, that context declarations must be set up according
to the needs and coding conventions of a particular system; different parts
of Nightshade are compiled with different context declarations, and your
system will probably need its own declarations.  In particular, any use of
the :match option depends on naming conventions used in coding.
]#

#[ Inline Expansion

\cindex{inline expansion}
\cpsubindex{expansion}{inline}
\cpsubindex{call}{inline}
\cpsubindex{function call}{inline}
\cpsubindex{optimization}{function call}

The compiler can expand almost any function inline, including functions
with keyword arguments.  The only restrictions are that keyword argument
keywords in the call must be constant, and that global function definitions
(`defun') must be done in a null lexical environment (versus being nested
in a `let' or other binding form).  Local functions (`flet') can be inline
expanded in any environment.  Combined with the compiler's source-level
optimization, inline expansion can be used for things that formerly
required macros for efficient implementation.  In the compiler, macros are
equivalent to functions in efficiency, so they need only be used where a
macro's syntactic flexibility is required.

Inline expansion is a compiler optimization technique that reduces the
overhead of a function call by simply not doing the call: instead, the
compiler effectively rewrites the program to appear as though the
definition of the called function was inserted at each call site.  In Lisp,
this is straightforwardly expressed by inserting the `lambda' corresponding
to the original definition:

    (proclaim '(inline my-1+))
    (defun my-1+ (x) (+ x 1))

    (my-1+ someval) ==> ((lambda (x) (+ x 1)) someval)

When the function expanded inline is large, the program after inline
expansion may be substantially larger than the original program.  If the
program becomes too large, inline expansion hurts speed rather than helping
it, since hardware resources such as physical memory and cache can be
exhausted.  Inline expansion is called for:

  * When profiling has shown that a relatively simple function is called so
    often that a large amount of time is being wasted in the calling of that
    function (as opposed to running in that function).  If a function is
    complex, it will take a long time to run relative to the time spent in
    the call, so the speed advantage of inline expansion is diminished at
    the same time the space cost of inline expansion is increased.  Of
    course, if a function is rarely called, then the overhead of calling it
    is also insignificant.

  * With functions so simple that they take less space to inline expand than
    would be taken to call the function (such as `my-1+' above).  It would
    require intimate knowledge of the compiler to be certain when inline
    expansion would reduce space, but it is generally safe to inline expand
    functions whose definition is a single function call, or a few calls to
    simple standard functions.

In addition to this speed/space tradeoff from inline expansion's avoidance
of the call, inline expansion can also reveal opportunities for
optimization.  The compiler's extensive source-level optimization can make
use of context information from the caller to tremendously simplify the
code resulting from the inline expansion of a function.

The main form of caller context is local information about the actual
argument values: what the argument types are and whether the arguments are
constant.  Knowledge about argument types can eliminate run-time type tests
(e.g., for generic arithmetic).  Constant arguments in a call provide
opportunities for constant folding optimization after inline expansion.

A hidden way that constant arguments are often supplied to functions is
through the defaulting of unsupplied optional or keyword arguments.  There
can be a huge efficiency advantage to inline expanding functions that have
complex keyword-based interfaces, such as this definition of the `member'
function:

    (proclaim '(inline member))

    (defun member (item list &key
                        (key #'identity)
                        (test #'eql testp)
                        (test-not nil notp))
      (do ((list list (cdr list)))
          ((null list) nil)
        (let ((car (car list)))
          (if (cond (testp
                     (funcall test item (funcall key car)))
                    (notp
                     (not (funcall test-not item (funcall key car))))
                    (t
                     (funcall test item (funcall key car))))
              (return list)))))

After inline expansion, this call is simplified to the obvious code:

    (member a l :key #'foo-a :test #'char=)

    ==>

    (do ((list list (cdr list)))
        ((null list) nil)
      (let ((car (car list)))
        (if (char= item (foo-a car))
            (return list))))

In this example, there could easily be more than an order of magnitude of
improvement in speed.  In addition to saving the original call to `member',
inline expansion also allows the calls to `char=' and `foo-a' to be
open-coded.  We go from a loop with three tests and two calls to a loop
with one test.

\xlref{source-optimization} for more discussion of source level
optimization.

[ Inline Expansion Recording   ]
[ Semi-Inline Expansion        ]
[ The Maybe-Inline Declaration ]
]#

#[ Inline Expansion Recording
\cindex{recording of inline expansions}

Inline expansion requires that the source for the inline expanded function
to be available when calls to the function are compiled.  The compiler
doesn't remember the inline expansion for every function, since that would
take an excessive about of space.  Instead, the programmer must tell the
compiler to record the inline expansion before the definition of the inline
expanded function is compiled.  This is done by globally declaring the
function inline before the function is defined, by using the \code{inline}
and \code{extensions:maybe-inline} (\pxlref{maybe-inline-declaration})
declarations.

In addition to recording the inline expansion of inline functions at the
time the function is compiled, `compile-file' also puts the inline
expansion in the output file.  When the output file is loaded, the inline
expansion is made available for subsequent compilations; there is no need
to compile the definition again to record the inline expansion.

If a function is declared inline, but no expansion is recorded, then the
compiler will give an efficiency note like:

    Note: MYFUN is declared inline, but has no expansion.

When you get this note, check that the \code{inline} declaration and the
definition appear before the calls that are to be inline expanded.  This
note will also be given if the inline expansion for a `defun' could not be
recorded because the `defun' was in a non-null lexical environment.
]#

#[ Semi-Inline Expansion

The compiler supports "semi-inline" functions.  Semi-inline expansion
shares a single copy of a function across all the calls in a component by
converting the inline expansion into a local function
(\pxlref{local-call}).  This takes up less space when there are multiple
calls, but also provides less opportunity for context dependent
optimization.  When there is only one call, the result is identical to
normal inline expansion.  Semi-inline expansion is done when the
\code{space} optimization quality is \code{0}, and the function has been
declared \code{extensions:maybe-inline}.

This mechanism of inline expansion combined with local call also allows
recursive functions to be inline expanded.  If a recursive function is
declared \code{inline}, calls will actually be compiled semi-inline.
Although recursive functions are often so complex that there is little
advantage to semi-inline expansion, it can still be useful in the same sort
of cases where normal inline expansion is especially advantageous, i.e.
functions where the calling context can help a lot.
]#

#[ The Maybe-Inline Declaration

\cindex{maybe-inline declaration}

The \code{extensions:maybe-inline} declaration is similar to \code{inline},
but indicates that inline expansion may sometimes be desirable, rather than
saying that inline expansion should almost always be done.  When used in a
global declaration, \code{extensions:maybe-inline} causes the expansion for
the named functions to be recorded, but the functions aren't actually
inline expanded unless \code{space} is \code{0} or the function is
eventually (perhaps locally) declared \code{inline}.

Use of the \code{extensions:maybe-inline} declaration followed by the
`defun' is preferable to the standard idiom of:

    (proclaim '(inline myfun))
    (defun myfun () ...)
    (proclaim '(notinline myfun))

    ;;; Any calls to \code{myfun} here are not inline expanded.

    (defun somefun ()
      (declare (inline myfun))
      ;;
      ;; Calls to \code{myfun} here are inline expanded.
      ...)

The problem with using \code{notinline} in this way is that in \clisp{} it
does more than just suppress inline expansion, it also forbids the compiler
to use any knowledge of \code{myfun} until a later \code{inline}
declaration overrides the \code{notinline}.  This prevents compiler
warnings about incorrect calls to the function, and also prevents block
compilation.

The \code{extensions:maybe-inline} declaration is used like this:
    (proclaim '(extensions:maybe-inline myfun))
    (defun myfun () ...)

    ;;; Any calls to \code{myfun} here are not inline expanded.

    (defun somefun ()
      (declare (inline myfun))
      ;;
      ;; Calls to \code{myfun} here are inline expanded.
      ...)

    (defun someotherfun ()
      (declare (optimize (space 0)))
      ;;
      ;; Calls to \code{myfun} here are expanded semi-inline.
      ...)

In this example, the use of \code{extensions:maybe-inline} causes the
expansion to be recorded when the \code{defun} for \code{somefun} is
compiled, and doesn't waste space through doing inline expansion by
default.  Unlike \code{notinline}, this declaration still allows the
compiler to assume that the known definition really is the one that will be
called when giving compiler warnings, and also allows the compiler to do
semi-inline expansion when the policy is appropriate.

When the goal is merely to control whether inline expansion is done by
default, it is preferable to use \code{extensions:maybe-inline} rather than
\code{notinline}.  The \code{notinline} declaration should be reserved for
those special occasions when a function may be redefined at run-time, so
the compiler must be told that the obvious definition of a function is not
necessarily the one that will be in effect at the time of the call.
]#

#[ Byte Coded Compilation
\cindex{byte coded compilation}
\cindex{space optimization}

The compiler supports byte compilation to reduce the size of programs by
allowing functions to be compiled more compactly.  Byte compilation
provides an extreme speed/space tradeoff: byte code is typically six times
more compact than native code, but runs fifty times (or more) slower.  This
is about ten times faster than the standard interpreter.

Large Lisp systems (such as Nightshade itself) often have large amounts of
user-interface code, compile-time (macro) code, debugging code, or rarely
executed special-case code.  This code is a good target for byte
compilation: very little time is spent running in it, but it can take up
quite a bit of space.  Straight-line code with many function calls is much
more suitable than inner loops.

When byte-compiling, the compiler compiles about twice as fast, and can
produce a hardware independent object file (.lbytef FIX or .bbytef? type).
This file can be loaded like a normal fasl file on any implementation of
Nightshade with the same byte-ordering. FIX hardware dependant?

The decision to byte compile or native compile can be done on a per-file or
per-code-object basis.  The \kwd{byte-compile} argument to
`compile-file' has these possible values:

  % ()

    Don't byte compile anything in this file.

  % #t

    Byte compile everything in this file and produce a
    processor-independent .bytef file.

  % :maybe

    Produce a normal fasl file, but byte compile any functions for which
    the \code{speed} optimization quality is \code{0} and the \code{debug}
    quality is less than or equal to \code{1}.

{variable:ext:*byte-compile-top-level*}
{variable:ext:*byte-compile-default*}
]#

#[ Object Representation
\cindex{object representation}
\cpsubindex{representation}{object}
\cpsubindex{efficiency}{of objects}

A somewhat subtle aspect of writing efficient Lisp programs is choosing the
data structures so that the underlying objects can be implemented
efficiently.  This is partly because of the need for multiple
representations for a given value ([Variables]), but is also due to the
sheer number of types that Lisp has built in.  The number of possible
representations complicates the choice of a good representation because
semantically similar types may vary in their efficiency depending on how
the program operates on them.

\cpsubindex{lists}{efficiency of}

In particular, lists have been superseded by other data types for many
purposes.

[ Structure Representation    ]
[ Arrays                      ]
[ Vectors                     ]
[ Bit-Vectors                 ]
[ Hashtables                  ]
]#

#[ Structure Representation

\cpsubindex{structure types}{efficiency of}

One of the best ways of building complex data structures is to define
appropriate structure types using `defstruct'.  In the compiler, access of
structure slots is always at least as fast as list or vector access, and is
usually faster.  In comparison to a list representation of a tuple,
structures also have a space advantage.

Even if structures weren't more efficient than other representations,
structure use would still be attractive because programs that use
structures in appropriate ways are much more maintainable and robust than
programs written using only lists.  For example:

    (rplaca (caddr (cadddr x)) (caddr y))

could have been written using structures in this way:

    (setf (beverage-flavor (astronaut-beverage x)) (beverage-flavor y))

The second version is more maintainable because it is easier to understand
what it is doing.  It is more robust because structures accesses are type
checked.  An \code{astronaut} will never be confused with a
\code{beverage}, and the result of \code{beverage-flavor} is always a
flavor.  See sections \ref{structure-types} and \ref{freeze-type} for more
information about structure types.  \xlref{type-inference} for a number of
examples that make clear the advantages of structure typing.

Note that the structure definition should be compiled before any uses of its
accessors or type predicate so that these function calls can be efficiently
open-coded.
]#

#[ Arrays
\cpsubindex{arrays}{efficiency of}

Arrays are often the most efficient representation for collections of
values because:

  * Array representations are often the most compact.  An array is always
    more compact than a list containing the same number of elements.

  * Arrays allow fast constant-time access.

  * Arrays are easily destructively modified, which can reduce consing.

  * Array element types can be specialized, which reduces both overall size
    and consing (\pxlref{specialized-array-types}).

Access of arrays that are not of type \code{simple-array} is less
efficient, so declarations are appropriate when an array is of a simple
type like \code{simple-string} or \code{simple-bit-vector}.  Arrays are
almost always simple, but the compiler may not be able to prove simpleness
at every use.  The only way to get a non-simple array is to use the
:displaced-to, :fill-pointer or :adjustable arguments to `make-array'.  If
you don't use these hairy options, then arrays can always be declared to be
simple.

Because of the many specialized array types and the possibility of
non-simple arrays, array access is much like generic arithmetic
(\pxlref{generic-arithmetic}).  In order for array accesses to be
efficiently compiled, the element type and simpleness of the array must be
known at compile time.  If there is inadequate information, the compiler is
forced to call a generic array access routine.  You can detect inefficient
array accesses by enabling efficiency notes, \pxlref{efficiency-notes}.
]#

#[ Vectors
\cpsubindex{vectors}{efficiency of}

Vectors (one dimensional arrays) are particularly useful, since in addition
to their obvious array-like applications, they are also well suited to
representing sequences.  In comparison to a list representation, vectors
are faster to access and take up between two and sixty-four times less
space (depending on the element type.)  As with arbitrary arrays, the
compiler needs to know that vectors are not complex, so you should use
\code{simple-string} in preference to \code{string}, etc.

The only advantage that lists have over vectors for representing sequences
is that it is easy to change the length of a list, add to it and remove
items from it.  Likely signs of archaic, slow lisp code are the use of
`nth' and `nthcdr'.  Code using these functions should probably be using a
vector.
]#

#[ Bit-Vectors
\cpsubindex{bit-vectors}{efficiency of}

Another thing that lists have been used for is set manipulation.  In
applications where there is a known, reasonably small universe of items
bit-vectors can be used to improve performance.  This is much less
convenient than using lists, because instead of symbols, each element in
the universe must be assigned a numeric index into the bit vector.  Using a
bit-vector will nearly always be faster, and can be tremendously faster if
the number of elements in the set is not small.  The logical operations on
\code{simple-bit-vector}s are efficient, since they operate on a word at a
time.
]#

#[ Hashtables
\cpsubindex{hash-tables}{efficiency of}

Hashtables are an efficient and general mechanism for maintaining
associations such as the association between a value and its name.
Although hashtables are usually the best way to maintain associations,
efficiency and style considerations sometimes favor the use of an
association list (alist).

`assoc' is fairly fast when the :test argument is `eq' or `eql' and there
are only a few elements, but the time goes up in proportion with the number
of elements.  In contrast, the hash-table lookup has a somewhat higher
overhead, but the speed is largely unaffected by the number of entries in
the table.  For an `equal' hash-table or alist, hash-tables have an even
greater advantage, since the test is more expensive.  In any case, be sure
to use the most restrictive test function possible.

The style argument observes that although hash-tables and alists overlap in
function, they do not do all things equally well.

  * Alists are good for maintaining scoped environments.  They were
    originally invented to implement scoping in the Lisp interpreter, and
    are still used for this in the compiler.  With an alist one can
    non-destructively change an association simply by consing a new element
    on the front.  This is something that cannot be done with hash-tables.

  * Hashtables are good for maintaining a global association.  The value
    associated with an entry can easily be changed with `setf'.  With an
    alist, one has to go through contortions, either `rplacd'ing the cons if
    the entry exists, or pushing a new one if it doesn't.  The
    side-effecting nature of hash-table operations is an advantage here.

Historically, symbol property lists were often used for global name
associations.  Property lists provide an awkward and error-prone
combination of name association and record structure.  If you must use the
property list, please store all the related values in a single structure
under a single property, rather than using many properties.  This makes
access more efficient, and also adds a modicum of typing and abstraction.
\xlref{advanced-type-stuff} for information on types in Nightshade.
]#

#[ Numbers
\cpsubindex{numeric}{types}
\cpsubindex{types}{numeric}

Numbers are interesting because numbers are one of the few Lisp data types
that have direct support in conventional hardware.  If a number can be
represented in the way that the hardware expects it, then there is a big
efficiency advantage.

Using hardware representations is problematical in Lisp due to dynamic
typing (where the type of a value may only be known at run (FIX or load?)
time).  It is possible to compile code for statically typed portions of a
Lisp program with efficiency comparable to that obtained in statically
typed languages such as C.  There are two main barriers to efficient
numerical code in Lisp:

  * The compiler must prove that the numerical expression is in fact
    statically typed, and

  * The compiler must be able to somehow reconcile the conflicting demands
    of the hardware mandated number representation with the Lisp
    requirements of dynamic typing and garbage-collecting dynamic storage
    allocation.

Because of its type inference (\pxlref{type-inference}) and efficiency
notes (\pxlref{efficiency-notes}), the compiler FIX? often succeeds at
ensuring that numerical expressions are statically typed.  The compiler
also goes some way into the area of allowing native machine number
representations in the presence of garbage collection.

[ Descriptors                    ]
[ Non-Descriptor Representations ]
[ Variables                      ]
[ Generic Arithmetic             ]
[ Fixnums                        ]
[ Word Integers                  ]
[ Floating Point Efficiency      ]
[ Specialized Arrays             ]
[ Specialized Structure Slots    ]
[ Interactions With Local Call   ]
[ Representation of Characters   ]
]#

#[ Descriptors
\cpsubindex{descriptors}{object}
\cindex{object representation}
\cpsubindex{representation}{object}
\cpsubindex{consing}{overhead of}

Lisp's dynamic typing requires that it be possible to represent any value
with a fixed length object, known as a descriptor.  This fixed-length
requirement is implicit in features such as:

  * Data types (like \code{simple-vector}) that can contain any type of
    object, and that can be destructively modified to contain different
    objects (of possibly different types.)

  * Functions that can be called with any type of argument, and that can be
    redefined at run time.

In order to save space, a descriptor is always represented as a single
word.  Types that can be directly represented in the descriptor itself are
said to be "immediate".  Descriptors for types larger than one word are in
reality pointers to the memory actually containing the value.

Representing values using pointers has two major negatives:

  * The memory pointed to must be allocated on the heap, so it must
    eventually be freed by the garbage collector.  Excessive heap allocation
    (or "consing") is inefficient in several ways.  \xlref{consing}.

  * Representing a value in memory requires the compiler to emit additional
    instructions to read the actual value in from memory, and then to write
    the value back after operating on it.

The introduction of garbage collection makes things even worse, since the
garbage collector must be able to determine whether a descriptor is
immediate or a pointer.  This requires that a few bits in each descriptor
be dedicated to the garbage collector.  The loss of a few bits doesn't seem
like much, but it has a major efficiency implication -- objects whose
natural machine representation is a full word (integers and single-floats)
must have a pointer representation.  So the compiler is forced to use an
artificial immediate representation (such as \code{fixnum}) or a natural
pointer representation (with the attendant consing overhead).
]#

#[ Non-Descriptor Representations
\cindex{non-descriptor representations}
\cindex{stack numbers}

From the discussion above, we can see that the standard descriptor
representation has many problems, the worst being number consing.  Lisp
compilers try to avoid these descriptor efficiency problems by using
"non-descriptor" representations.  A compiler that uses non-descriptor
representations can compile this function so that it does no number
consing:

    (defun multby (vec n)
      (declare (type (simple-array single-float (*)) vec)
               (single-float n))
      (dotimes (i (length vec))
        (setf (aref vec i)
              (* n (aref vec i)))))

If a descriptor representation were used, each iteration of the loop might
cons two floats and do three times as many memory references.

As its negative definition suggests, the range of possible non-descriptor
representations is large.  The performance improvement from non-descriptor
representation depends upon both the number of types that have
non-descriptor representations and the number of contexts in which the
compiler is forced to use a descriptor representation.

Many traditional Lisp compilers supported non-descriptor representations
for float types such as \code{single-float} and \code{double-float}
(section \ref{float-efficiency}).  The Nightshade compiler also supports
full word integers (\pxlref{word-integers}), characters
(\pxlref{characters}) and system-area pointers (unconstrained pointers,
\pxlref{system-area-pointers}).  Many traditional Lisp compilers supported
non-descriptor representations for variables (section \ref{ND-variables})
and array elements (section \ref{specialized-array-types}).  The Nightshade
compiler also supports non-descriptor arguments and return values in local
call (\pxlref{number-local-call}) and structure slots (\pxlref{raw-slots}).
]#

#[ Variables
\cpsubindex{variables}{non-descriptor}
\cpsubindex{type declarations}{variable}
\cpsubindex{efficiency}{of numeric variables}

In order to use a non-descriptor representation for a variable or
expression intermediate value, the compiler must be able to prove that the
value is always of a particular type having a non-descriptor
representation.  Type inference (\pxlref{type-inference}) often needs some
help from user-supplied declarations.  The best kind of type declaration is
a variable type declaration placed at the binding point:
    (let ((x (car l)))
      (declare (single-float x))
      ...)

Use of \code{the}, or of variable declarations not at the binding form is
insufficient to allow non-descriptor representation of the variable -- with
these declarations it is not certain that all values of the variable are of
the right type.  It is sometimes useful to introduce a gratuitous binding
that allows the compiler to change to a non-descriptor representation,
like:
    (etypecase x
      ((signed-byte 32)
       (let ((x x))
         (declare (type (signed-byte 32) x))
         ...))
      ...)

The declaration on the inner \code{x} is necessary here due to a phase
ordering problem.  Although the compiler will eventually prove that the
outer \code{x} is a \code{(signed-byte 32)} within that `etypecase' branch,
the inner \code{x} would have been optimized away by that time.  Declaring
the type makes let optimization more cautious.

FIX NB (is this the best place for this?)

Note that storing a value into a global (or \code{special}) variable always
forces a descriptor representation.  Wherever possible, you should operate
only on local variables, binding any referenced globals to local variables
at the beginning of the function, and doing any global assignments at the
end.

Efficiency notes signal use of inefficient representations, so programmer's
needn't continuously worry about the details of representation selection
(\pxlref{representation-eff-note}.)
]#

#[ Generic Arithmetic
\cindex{generic arithmetic}
\cpsubindex{arithmetic}{generic}
\cpsubindex{numeric}{operation efficiency}

In Nightshade Lisp, arithmetic operations are "generic".  The `+' function
can be passed \code{fixnum}s, \code{bignum}s, \code{ratio}s, and various
kinds of \code{float}s and \code{complex}es, in any combination.  In
addition to the inherent complexity of \code{bignum} and \code{ratio}
operations, there is also a lot of overhead in just figuring out which
operation to do and what contagion (FIX first mention?) and
canonicalization rules apply.  The complexity of generic arithmetic is so
great that it is inconceivable to open code it.  Instead, the compiler does
a function call to a generic arithmetic routine, consuming many
instructions before the actual computation even starts.

This is a poor show, since even Lisp programs do a lot of arithmetic, and
the hardware is usually capable of doing operations on small integers and
floats with a single instruction.  To get acceptable efficiency, the
compiler special-cases uses of generic arithmetic that are directly
implemented in the hardware.  In order to open code arithmetic, several
constraints must be met:

  * All the arguments must be known to be a good type of number.

  * The result must be known to be a good type of number.

  * Any intermediate values such as the result of \code{(+ a b)} in the
    call \code{(+ a b c)} must be known to be a good type of number.

  * All the above numbers with good types must be of the \var{same} good
    type.  Don't try to mix integers and floats or different float formats.

The "good types" are \code{(signed-byte 32)}, \code{(unsigned-byte 32)},
\code{single-float} and \code{double-float}.  See sections \ref{fixnums},
\ref{word-integers} and \ref{float-efficiency} for more discussion of good
numeric types.

\code{float} is not a good type, since it might mean either
\code{single-float} or \code{double-float}.  \code{integer} is not a good
type, since it might mean \code{bignum}.  \code{rational} is not a good
type, since it might mean \code{ratio}.  Note however that these types are
still useful in declarations, since type inference may be able to
strengthen a weak declaration into a good one, when it would be at a loss
if there was no declaration at all (\pxlref{type-inference}).  The
\code{integer} and \code{unsigned-byte} (or non-negative integer) types are
especially useful in this regard, since they can often be strengthened to a
good integer type.

Arithmetic with \code{complex} numbers is inefficient in comparison to
float and integer arithmetic.  Complex numbers are always represented with
a pointer descriptor (causing consing overhead), and complex arithmetic is
always closed coded using the general generic arithmetic functions.  But
arithmetic with complex types such as:

    (complex float)
    (complex fixnum)

is still faster than \code{bignum} or \code{ratio} arithmetic, since the
implementation is much simpler.

Note: don't use `/' to divide integers unless you want the overhead of
rational arithmetic.  Use `truncate' even when you know that the arguments
divide evenly.

Programmers don't need to remember all the rules for how to get open-coded
arithmetic, since efficiency notes explain when and where there is a
problem -- \pxlref{efficiency-notes}.
]#

#[ Fixnums
\cindex{fixnums}
\cindex{bignums}

A fixnum is a "FIXed precision NUMber".  In modern Lisp implementations,
fixnums can be represented with an immediate descriptor, so operating on
fixnums requires no consing or memory references.  Clever choice of
representations also allows some arithmetic operations to be done on
fixnums using hardware supported word-integer instructions, somewhat
reducing the speed penalty for using an artificial integer representation.

It is useful to distinguish the \code{fixnum} type from the fixnum
representation of integers.  In the compiler, \code{fixnum} type is the
same as other finite integer types.  \code{fixnum} is equivalent to (i.e.
is defined with `deftype' to be) \code{(signed-byte 30)}.  \code{fixnum} is
simply the largest subset of integers that can be represented using an
immediate fixnum descriptor.

In declarations in the Nightshade compiler it is always preferable to use
the more restrictive integer types such as \code{bit}, \code{(integer -43
7)} and \code{(unsigned-byte 8)} instead of the more general \code{fixnum}.
Since the compiler does understand these integer types the more restrictive
type allows better type inference (\pxlref{operation-type-inference}).

The small, efficient fixnum is contrasted with bignum, or "BIG NUMber".
This is another descriptor representation for integers, but this time a
pointer representation that allows for arbitrarily large integers.  Bignum
operations are less efficient than fixnum operations, both because of the
consing and memory reference overheads of a pointer descriptor, and also
because of the inherent complexity of extended precision arithmetic.  While
fixnum operations can often be done with a single instruction, bignum
operations are so complex that they are always done using generic
arithmetic.

A crucial point is that the compiler will use generic arithmetic if it
can't prove that all the arguments, intermediate values, and results are
fixnums.  With bounded integer types such as \code{fixnum}, the result type
proves to be especially problematical, since these types are not closed
under common arithmetic operations such as `+', `-', `*' and `/'.  For
example, \code{(1+ (the fixnum x))} does not necessarily evaluate to a
\code{fixnum}.  Bignums were added to Lisp to get around this problem, but
they really just transform the correctness problem "if this add overflows,
you will get the wrong answer" to the efficiency problem "if this add
\var{might} overflow then your program will run slowly (because of generic
arithmetic)."

There is just no getting around the fact that the hardware only directly
supports short integers.  To get the most efficient open coding, the
compiler must be able to prove that the result is a good integer type.
This is an argument in favor of using more restrictive integer types:
\code{(1+ (the fixnum x))} may not always be a \code{fixnum}, but \code{(1+
(the (unsigned-byte 8) x))} always is.  Of course, putting in lots of
\code{the} declarations and then compiling with \code{safety} \code{0} can
also assert the result type.
]#

#[ Word Integers
\cindex{word integers}

The compiler efficiently implements arithmetic on full-word integers
through non-descriptor representations and open coding.  Arithmetic on any
subtype of these types:

    (signed-byte 32)
    (unsigned-byte 32)

is reasonably efficient, although subtypes of \code{fixnum} remain somewhat
more efficient.

If a word integer must be represented as a descriptor, then the
\code{bignum} representation is used, with its associated consing overhead.
The support for word integers in no way changes the language semantics, it
just makes arithmetic on small bignums vastly more efficient.  It is fine
to do arithmetic operations with mixed \code{fixnum} and word integer
operands; just declare the most specific integer type you can, and let the
compiler decide what representation to use.

In fact, to most users, the greatest advantage of word integer arithmetic is
that it effectively provides a few guard bits on the fixnum representation.  If
there are missing assertions on intermediate values in a fixnum expression, the
intermediate results can usually be proved to fit in a word.  After the whole
expression is evaluated, there will often be a fixnum assertion on the final
result, allowing creation of a fixnum result without even checking for
overflow.

The remarks in section \ref{fixnums} about fixnum result type also apply to
word integers; you must be careful to give the compiler enough information
to prove that the result is still a word integer.  This time, though, when
we blow out of word integers we land in into generic bignum arithmetic,
which is much worse than sleazing from \code{fixnum}s to word integers.
Note that mixing \code{(unsigned-byte 32)} arguments with arguments of any
signed type (such as \code{fixnum}) is a no-no, since the result might not
be unsigned.
]#

#[ Floating Point Efficiency
\cindex{floating point efficiency}

Arithmetic on objects of type \code{single-float} and \code{double-float}
is efficiently implemented using non-descriptor representations and open
coding.  As for integer arithmetic, the arguments must be known to be of
the same float type.  In contrast to integer arithmetic, the results and
intermediate values usually take care of themselves due to the rules of
float contagion, i.e. \code{(1+ (the single-float x))} is always a
\code{single-float}.

Although they are not specially implemented, \code{short-float} and
\code{long-float} are also acceptable in declarations, since they are
synonyms for the \code{single-float} and \code{double-float} types,
respectively.  It is harmless to use list-style float type specifiers such
as \code{(single-float 0.0 1.0)}, but the compiler currently makes little
use of bounds on float types.

When a float must be represented as a descriptor, a pointer representation
is used, creating consing overhead.  For this reason, you should try to
avoid situations (such as full call and non-specialized (FIX generic?) data
structures) that force a descriptor representation.  See sections
\ref{specialized-array-types}, \ref{raw-slots} and \ref{number-local-call}.

\xlref{ieee-float} for information on the extensions to support IEEE
floating point.
]#

#[ Specialized Arrays
\cindex{specialized array types}
\cpsubindex{array types}{specialized}
\cpsubindex{types}{specialized array}

Nightshade Lisp supports specialized array element types through the
:element-type argument to `make-array'.  When an array has a specialized
element type, only elements of that type can be stored in the array.  From
this restriction comes two major efficiency advantages:

  * A specialized array can save space by packing multiple elements into a
    single word.  For example, a \code{base-char} array can have 4 elements
    per word, and a \code{bit} array can have 32.  This space-efficient
    representation is possible because it is not necessary to separately
    indicate the type of each element.

  * The elements in a specialized array can be given the same non-descriptor
    representation as the one used in registers and on the stack,
    eliminating the need for representation conversions when reading and
    writing array elements.  For objects with pointer descriptor
    representations (such as floats and word integers) there is also a
    substantial consing reduction because it is not necessary to allocate a
    new object every time an array element is modified.

These are the specialized element types currently supported:

    bit
    (unsigned-byte 2)
    (unsigned-byte 4)
    (unsigned-byte 8)
    (unsigned-byte 16)
    (unsigned-byte 32)
    base-character
    single-float
    double-float

Although a \code{simple-vector} can hold any type of value, #t should still
be considered a specialized array type, since arrays with element type #t
are specialized to hold descriptors.

When using non-descriptor representations, it is particularly important to
make sure that array accesses are open-coded, since in addition to the
generic operation overhead, efficiency is lost when the array element is
converted to a descriptor so that it can be passed to (or from) the generic
access routine.  Enabling efficiency notes, \pxlref{efficiency-notes}
provides detection of inefficient array accesses.  \xlref{array-types}.
]#

#[ Specialized Structure Slots
\cpsubindex{structure types}{numeric slots}
\cindex{specialized structure slots}

Structure slots declared by the :type `defstruct' slot option to have
certain known numeric types are also given non-descriptor representations.
These types (and subtypes of these types) are supported:

    (unsigned-byte 32)
    single-float
    double-float

The primary advantage of specialized slot representations is a large
reduction spurious memory allocation and access overhead of programs that
intensively use these types.
]#

#[ Interactions With Local Call
\cpsubindex{local call}{numeric operands}
\cpsubindex{call}{numeric operands}
\cindex{numbers in local call}

Local call has many advantages (\pxlref{local-call}); one relevant to the
discussion here is that local call extends the usefulness of non-descriptor
representations.  If the compiler knows from the argument type that an
argument has a non-descriptor representation, then the argument will be
passed in that representation.  The easiest way to ensure that the argument
type is known at compile time is to always declare the argument type in the
called function, like:
    (defun 2+f (x)
      (declare (single-float x))
      (+ x 2.0))

The advantages of passing arguments and return values in a non-descriptor
representation are the same as for non-descriptor representations in
general: reduced consing and memory access (\pxlref{non-descriptor}.)  This
extends the applicative programming styles discussed in section
\ref{local-call} to numeric code.  Also, if source files are kept
reasonably small, block compilation can be used to reduce number consing to
a minimum.

Note that non-descriptor return values can only be used with the known
return convention (section \ref{local-call-return}.)  If the compiler can't
prove that a function always returns the same number of values, then it
must use the unknown values return convention, which requires a descriptor
representation.  Pay attention to the known return efficiency notes to
avoid number consing.
]#

#[ Representation of Characters
\cindex{characters}
\cindex{strings}

The compiler also uses a non-descriptor representation for characters when
convenient.  This improves the efficiency of string manipulation, but is
otherwise pretty hidden; characters have an immediate descriptor
representation, so there is not a great penalty for converting a character
to a descriptor.  Nonetheless, it may sometimes be helpful to declare
character-valued variables as \code{base-character}.
]#

#[ General Efficiency Hints
\cpsubindex{efficiency}{general hints}

This section is a summary of various implementation costs and ways to get
around them.  These hints are relatively unrelated to the use of the
compiler, and probably also apply to most other Lisp implementations.  In
each section, there are references to related in-depth discussion.

[ Compile Your Code           ]
[ Avoid Unnecessary Consing   ]
[ Complex Argument Syntax     ]
[ Mapping and Iteration       ]
[ Trace Files and Disassembly ]
]#

#[ Compile Your Code
\cpsubindex{compilation}{why to}

At this point, the advantages of compiling code relative to running it
interpreted is probably apparent.  Remember that in Nightshade, compiled
code typically runs hundreds of times faster than interpreted code.  Also,
compiled (FASL) files load significantly faster than source files, so it is
worthwhile compiling files which are loaded many times, even if the
functions in the file can run slowly.

Aside from the efficiency advantages, compiled code is as good or better
than interpreted code.  Compiled code can be inspected at the source level
(see chapter \ref{debugger}) at run time, and compiled code does more error
checking.  For these reasons, the interpreter should be regarded mainly as
an interactive command interpreter, rather than as a programming language
implementation.

Be concerned about the performance of your program only after seeing its
speed compiled.  Some techniques that make compiled code run faster make
interpreted code run slower.
]#

#[ Avoid Unnecessary Consing
\cindex{consing}
\cindex{garbage collection}
\cindex{memory allocation}
\cpsubindex{efficiency}{of memory use}

The `cons' function (FIX hence its name).  Many other functions also cons,
including `make-array'.  Arithmetic and function call can also have hidden
consing overheads.  Consing hurts performance in the following ways:

  * Consing reduces memory access locality, increasing paging activity.

  * Consing takes time just like anything else.

  * Any space allocated eventually needs to be reclaimed, either by garbage
    collection or by starting a new Lisp process.

Consing has some good aspects, since programs do things other than consing,
and appropriate consing can speed up the real work.  It would certainly
save time to allocate a vector of intermediate results that are reused
hundreds of times.  Also, if it is necessary to copy a large data structure
many times, it may be more efficient to update the data structure in-place;
this somewhat increases update overhead, but makes copying trivial.

Note that the remarks in section \ref{efficiency-overview} about the
importance of separating tuning from coding also apply to consing overhead.
The majority of consing will be done by a small portion of the program.
The consing hot spots are even less predictable than the CPU hot spots.
During initial coding, avoid unnecessary side-effects and cons where it is
convenient.  If profiling reveals a consing problem, then go back and fix
the hot spots.

\xlref{non-descriptor} for a discussion of how to work around number
consing.
]#

#[ Complex Argument Syntax
\cpsubindex{argument syntax}{efficiency}
\cpsubindex{efficiency}{of argument syntax}
\cindex{keyword argument efficiency}
\cindex{rest argument efficiency}

Nightshade Lisp has very powerful argument passing mechanisms.  Two of the
most powerful mechanisms, rest arguments and keyword arguments, have a
significant performance penalty:

  * With keyword arguments, the called function has to parse the supplied
    keywords by iterating over them and checking them against the desired
    keywords.

  * With rest arguments, the function must cons a list to hold the
    arguments.  If a function is called many times or with many arguments,
    large amounts of memory will be allocated.

Although rest argument consing is worse than keyword parsing, either is
only serious when thousands of calls are made to such a function.  The use
of keyword arguments is strongly encouraged in functions with many
arguments or with interfaces that are likely to be extended, and rest
arguments are often natural in user interface functions.

Optional arguments have some efficiency advantage over keyword arguments,
but their syntactic clumsiness and lack of extensibility has caused many
Common Lisp programmers to use optionals only in functions that have
obviously simple and fixed interfaces (such as `subseq'), or in functions
that are only called in a few places.  When defining an interface function
to be used by other programmers or users, use of only required and keyword
arguments is recommended.

Parsing of `defmacro' keyword and rest arguments is done at compile time,
so a macro can be used to provide a convenient syntax with an efficient
implementation.  If the macro-expanded form contains no keyword or rest
arguments, then it is perfectly acceptable in inner loops.

Keyword argument parsing overhead can also be worked around by use of
inline expansion (\pxlref{inline-expansion}) and block compilation (section
\ref{block-compilation}).

Note: the compiler open-codes most heavily used system functions which have
keyword or rest arguments, to save these run-time overheads.
]#

#[ Mapping and Iteration
\cpsubindex{mapping}{efficiency of}

One of the traditional Lisp programming styles is a highly applicative one,
involving the use of mapping functions and many lists to store intermediate
results.  To compute the sum of the square-roots of a list of numbers, one
might say:
    (apply #'+ (mapcar #'sqrt list-of-numbers))

This programming style is clear and elegant, but results in slow code.
There are two reasons why:

  * The creation of lists of intermediate results causes much consing (see
    \ref{consing}).

  * Each level of application requires another scan down the list.  Thus,
    disregarding other effects, the above code would probably take twice as
    long as a straightforward iterative version.

An example of an iterative version of the same code:
    (do ((num list-of-numbers (cdr num))
         (sum 0 (+ (sqrt (car num)) sum)))
        ((null num) sum))

See sections \ref{variable-type-inference} and \ref{let-optimization} for a
discussion of the interactions of iteration constructs with type inference
and variable optimization.  Also, section \ref{local-tail-recursion}
discusses an alternative applicative style of iteration.  ; FIX maybe show this eg there
]#

#[ Trace Files and Disassembly
\cindex{trace files}
\cindex{assembly listing}
\cpsubindex{listing files}{trace}
\cindex{Virtual Machine (VM, or IR2) representation}
\cindex{implicit continuation representation (IR1)}
\cpsubindex{continuations}{implicit representation}

In order to write efficient code, the programmer needs to know the relative
costs of different operations.  The main challenge in writing efficient
Lisp code is that there are so many operations, and the costs of these
operations vary in obscure context-dependent ways.  Although efficiency
notes point out some problem areas, the only way to ensure generation of
the best code is to look at the assembly code output.

The `disassemble' function is a convenient way to get the assembly code for
a function, but it can be very difficult to interpret, since the
correspondence with the original source code is weak.  A better (but more
awkward) option is to use the :trace-file argument to `compile-file' to
generate a trace file.

A trace file is a dump of the compiler's internal representations,
including annotated assembly code.  Each component in the program gets four
pages in the trace file (separated by "^L"):

  * The implicit-continuation (or IR1) representation of the optimized
    source.  This is a dump of the flow graph representation used for
    "source level" optimizations.  As you will quickly notice, it is not
    really very close to the source.  This representation is not very useful
    to even sophisticated users.

  * The Virtual Machine (VM, or IR2) representation of the program.  This dump
    represents the generated code as sequences of "Virtual OPerations"
    (VOPs.)  This representation is intermediate between the source and the
    assembly code -- each VOP corresponds fairly directly to some primitive
    function or construct, but a given VOP also has a fairly predictable
    instruction sequence.  An operation (such as `+') may have multiple
    implementations with different cost and applicability.  The choice of a
    particular VOP such as \code{+/fixnum} or \code{+/single-float}
    represents this choice of implementation.  Once you are familiar with
    it, the VM representation is probably the most useful for determining
    what implementation has been used.

  * An assembly listing, annotated with the VOP responsible for generating the
    instructions.  This listing is useful for figuring out what a VOP does
    and how it is implemented in a particular context, but its large size
    makes it more difficult to read.

  * A disassembly of the generated code, which has all pseudo-operations expanded
    out, but is not annotated with VOPs.

Note that trace file generation takes much space and time, since the trace
file is tens of times larger than the source file.  It may be best to
separate the critical program portion into its own file and then generate
the trace file from this small file.
]#

#[ Efficiency Notes
\cindex{efficiency notes}
\cpsubindex{notes}{efficiency}
\cindex{tuning}

Efficiency notes are messages that warn the user that the compiler has
chosen a relatively inefficient implementation for some operation.  Usually
an efficiency note reflects the compiler's desire for more type
information.  If the type of the values concerned is known to the
programmer, then additional declarations can be used to get a more
efficient implementation.

Efficiency notes are controlled by the \code{extensions:inhibit-warnings}
optimization quality (\pxlref{optimize-declaration}.)  When \code{speed} is
greater than \code{extensions:inhibit-warnings}, efficiency notes are
enabled.  Note that this implicitly enables efficiency notes whenever
\code{speed} is increased from its default of \code{1}.

Consider this program with an obscure missing declaration:

    (defun eff-note (x y z)
      (declare (fixnum x y z))
      (the fixnum (+ x y z)))

If compiled with \code{(speed 3) (safety 0)}, this note is given:

    In: DEFUN EFF-NOTE
      (+ X Y Z)
    ==>
      (+ (+ X Y) Z)
    Note: Forced to do inline (signed-byte 32) arithmetic (cost 3).
          Unable to do inline fixnum arithmetic (cost 2) because:
          The first argument is a (INTEGER -1073741824 1073741822),
          not a FIXNUM.

This efficiency note tells us that the result of the intermediate
computation \code{(+ x y)} is not known to be a \code{fixnum}, so the
addition of the intermediate sum to \code{z} must be done less efficiently.
This can be fixed by changing the definition of `eff-note':

    (defun eff-note (x y z)
      (declare (fixnum x y z))
      (the fixnum (+ (the fixnum (+ x y)) z)))

[ Type Uncertainty                   ]
[ Efficiency Notes and Type Checking ]
[ Representation Efficiency Notes    ]
[ Verbosity Control                  ]
]#

#[ Type Uncertainty
\cpsubindex{types}{uncertainty}
\cindex{uncertainty of types}

The main cause of inefficiency is the compiler's lack of adequate
information about the types of function argument and result values.  Many
important operations (such as arithmetic) have an inefficient general
(generic) case, but have efficient implementations that can usually be used
if there is sufficient argument type information.

Type efficiency notes are given when a value's type is uncertain.  There is
an important distinction between values that are \i{not known} to be of a
good type (uncertain) and values that are \i{known not} to be of a good
type.  Efficiency notes are given mainly for the first case (uncertain
types.)  If it is clear to the compiler that that there is not an efficient
implementation for a particular function call, then an efficiency note will
only be given if the \code{extensions:inhibit-warnings} optimization
quality is \code{0} (\pxlref{optimize-declaration}.)

In other words, the default efficiency notes only suggest that you add
declarations, not that you change the semantics of your program so that an
efficient implementation will apply.  For example, compilation of this form
will not give an efficiency note:
    (elt (the list l) i)

even though a vector access is more efficient than indexing a list.
]#

#[ Efficiency Notes and Type Checking
\cpsubindex{type checking}{efficiency of}
\cpsubindex{efficiency}{of type checking}
\cpsubindex{optimization}{type check}

It is important that the `eff-note' example above (in (Efficiency Notes])
used \code{(safety 0)}.  When type checking is enabled, you may get
apparently spurious efficiency notes.  With \code{(safety 1)}, the note has
this extra line on the end:

     The result is a (INTEGER -1610612736 1610612733), not a FIXNUM.

This seems strange, since there is a `the' declaration on the result of
that second addition.

In fact, the inefficiency is real, and is a consequence of the compiler's
treating declarations as assertions to be verified.  The compiler can't
assume that the result type declaration is true -- it must generate the
result and then test whether it is of the appropriate type.

In practice, this means that when you are tuning a program to run without
type checks, you should work from the efficiency notes generated by unsafe
compilation.  If you want code to run efficiently with type checking, then
you should pay attention to all the efficiency notes that you get during
safe compilation.  Since user supplied output type assertions (e.g., from
`the') are disregarded when selecting operation implementations for safe
code, you must somehow give the compiler information that allows it to
prove that the result truly must be of a good type.  In our example, it
could be done by constraining the argument types more:

    (defun eff-note (x y z)
      (declare (type (unsigned-byte 18) x y z))
      (+ x y z))

Of course, this declaration is acceptable only if the arguments to
`eff-note' always are \code{(unsigned-byte 18)} integers.
]#

#[ Representation Efficiency Notes
\cindex{representation efficiency notes}
\cpsubindex{efficiency notes}{for representation}
\cindex{object representation efficiency notes}
\cindex{stack numbers}
\cindex{non-descriptor representations}
\cpsubindex{descriptor representations}{forcing of}

When operating on values that have non-descriptor representations
(\pxlref{non-descriptor}), there can be a substantial time and consing
penalty for converting to and from descriptor representations.  For this
reason, the compiler gives an efficiency note whenever it is forced to do a
representation coercion more expensive than
*efficiency-note-cost-threshold*.

Inefficient representation coercions may be due to type uncertainty, as in
this example:

    (defun set-flo (x)
      (declare (single-float x))
      (prog ((var 0.0))
        (setq var (gorp))
        (setq var x)
        (return var)))

which produces this efficiency note:

    In: DEFUN SET-FLO
      (SETQ VAR X)
    Note: Doing float to pointer coercion (cost 13) from X to VAR.

The variable \code{var} is not known to always hold values of type
\code{single-float}, so a descriptor representation must be used for its
value.  In this sort of situation, adding a declaration will eliminate the
inefficiency.

Often inefficient representation conversions are not due to type
uncertainty -- instead, they result from evaluating a non-descriptor
expression in a context that requires a descriptor result:

  * Assignment to or initialization of any data structure other than a
    specialized array (\pxlref{specialized-array-types}), or

  * Assignment to a \code{special} variable, or

  * Passing as an argument or returning as a value in any function call that
    is not a local call (\pxlref{number-local-call}).

If such inefficient coercions appear in a "hot spot" in the program, data
structures redesign or program reorganization may be necessary to improve
efficiency.  See sections \ref{block-compilation}, \ref{numeric-types} and
\ref{profiling}.

Because representation selection is done rather late in compilation, the
source context in these efficiency notes is somewhat vague, making
interpretation more difficult.  This is a fairly straightforward example:

    (defun cf+ (x y)
      (declare (single-float x y))
      (cons (+ x y) t))

which gives this efficiency note:

    In: DEFUN CF+
      (CONS (+ X Y) T)
    Note: Doing float to pointer coercion (cost 13), for:
          The first argument of CONS.

The source context form is almost always the form that receives the value
being coerced (as it is in the preceding example), but can also be the
source form which generates the coerced value.  Compiling this example:

    (defun if-cf+ (x y)
      (declare (single-float x y))
      (cons (if (grue) (+ x y) (snoc)) t))

produces this note:

    In: DEFUN IF-CF+
      (+ X Y)
    Note: Doing float to pointer coercion (cost 13).

In either case, the note's text explanation attempts to include additional
information about what locations are the source and destination of the
coercion.  Here are some example notes:
      (IF (GRUE) X (SNOC))
    Note: Doing float to pointer coercion (cost 13) from X.

      (SETQ VAR X)
    Note: Doing float to pointer coercion (cost 13) from X to VAR.

Note that the return value of a function is also a place to which coercions
may have to be done:

      (DEFUN F+ (X Y) (DECLARE (SINGLE-FLOAT X Y)) (+ X Y))
    Note: Doing float to pointer coercion (cost 13) to "<return value>".

Sometimes the compiler is unable to determine a name for the source or
destination, in which case the source context is the only clue.
]#

#[ Verbosity Control
\cpsubindex{verbosity}{of efficiency notes}
\cpsubindex{efficiency notes}{verbosity}

These variables control the verbosity of efficiency notes:

{variable:*efficiency-note-cost-threshold*}
{variable:*efficiency-note-limit*}
]#
