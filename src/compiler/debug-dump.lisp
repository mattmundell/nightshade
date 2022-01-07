;;; Stuff that creates debugger information from the compiler's internal
;;; data structures.

(in-package :c)

(defvar *byte-buffer*)
(declaim (type (vector (unsigned-byte 8)) *byte-buffer*))

#[ Debugger Information

Although the compiler's great freedom in choice of function call conventions
and variable representations has major efficiency advantages, it also has
unfortunate consequences for the debugger.  The debug information that we need
is even more elaborate than for conventional "compiled" languages, since we
cannot even do a simple backtrace without some debug information.  However,
once having gone this far, it is not that difficult to go the extra distance,
and provide full source level debugging of compiled code.

Full debug information has a substantial space penalty, so we allow different
levels of debug information to be specified.  In the extreme case, we can
totally omit debug information.


== The Debug-Info Structure ==

The Debug-Info structure directly represents information about the
source code, and points to other structures that describe the layout of
run-time data structures.


Make some sort of minimal debug-info format that would support at least the
common cases of level 1 (since that is what we would release), and perhaps
level 0.  Actually, it seems it wouldn't be hard to crunch nearly all of the
debug-function structure and debug-info function map into a single byte-vector.
We could have an uncrunch function that restored the current format.  This
would be used by the debugger, and also could be used by purify to delete parts
of the debug-info even when the compiler dumps it in crunched form.
    Note that this isn't terribly important if purify is smart about debug-info...


== Compiled source map representation ==

    XXX store in debug-function PC at which env is properly initialized, i.e.
    args (and return-pc, etc.) in internal locations.  This is where a
    :function-start breakpoint would break.

    XXX Note that that we can easily cache the form-number => source-path or
    form-number => form translation using a vector indexed by form numbers that we
    build during a walk.


Instead of using source paths in the debug-info, use "form numbers".  The form
number of a form is the number of forms that we walk to reach that form when
doing a pre-order walk of the source form.
    Might want to use a post-order walk, as that would more closely approximate
    evaluation order.


We probably want to continue using source-paths in the compiler, since they are
quick to compute and to get you to a particular form.
    XXX But actually, I
    guess we don't have to precompute the source paths and annotate nodes with
    them: instead we could annotate the nodes with the actual original source form.
    Then if we wanted to find the location of that form, we could walk the root
    source form, looking that original form.  But we might still need to enter all
    the forms in a hashtable so that we can tell during IR1 conversion that a given
    form appeared in the original source.]


Note that form numbers have an interesting property: it is quite efficient to
determine whether an arbitrary form is a subform of some other form, since the
form number of B will be > than A's number and < A's next sibling's number iff
B is a subform of A.

This should be quite useful for doing the source=>pc mapping in the debugger,
since that problem reduces to finding the subset of the known locations that
are for subforms of the specified form.


Assume a byte vector with a standard variable-length integer format, something
like this:
    0..253 => the integer
    254 => read next two bytes for integer
    255 => read next four bytes for integer

Then a compiled debug block is just a sequence of variable-length integers in a
particular order, something like this:
    number of successors
    ...offsets of each successor in the function's blocks vector...
    first PC
        offset of first top-level form (in forms) (only if not component default)
    form number of first source form
    first live mask (length in bytes determined by number of VARIABLES)
    ...more <PC, top-level form offset, form-number, live-set> tuples...

We determine the number of locations recorded in a block by the finding the
start of the next compiled debug block in the blocks vector.

    XXX Actually, only need 2 bits for number of successors {0,1,2}.  We might
    want to use other bits in the first byte to indicate the kind of location.
    XXX We could support local packing by having a general concept of "alternate
    locations" instead of just regular and save locations.  The location would have
    a bit indicating that there are alternate locations, in which case we read the
    number of alternate locations and then that many more SC-OFFSETs.  In the
    debug-block, we would have a second bit mask with bits set for TNs that are in
    an alternate location.  We then read a number for each such TN, with the value
    being interpreted as an index into the Location's alternate locations.



It looks like using structures for the compiled-location-info is too bulky.
Instead we need some packed binary representation.

First, let's represent a SC/offset pair with an "SC-Offset", which is an
integer with the SC in the low 5 bits and the offset in the remaining bits:
    ----------------------------------------------------
    | Offset (as many bits as necessary) | SC (5 bits) |
    ----------------------------------------------------
Probably the result should be constrained to fit in a fixnum, since it will be
more efficient and gives more than enough possible offsets.

We can the represent a compiled location like this:
    single byte of boolean flags:
	uninterned name
	packaged name
	environment-live
	has distinct save location
        has ID (name not unique in this fun)
    name length in bytes (as var-length integer)
    ...name bytes...
	if packaged, var-length integer that is package name length]
	 ...package name bytes...
        If has ID, ID as var-length integer
    SC-Offset of primary location (as var-length integer)
        If has save SC, SC-Offset of save location (as var-length integer)




But for a whizzy breakpoint facility, we would need a good source=>code map.
Dumping a complete code=>source map might be as good a way as any to represent
this, due to the one-to-many relationship between source and code locations.

We might be able to get away with just storing the source locations for the
beginnings of blocks and maintaining a mapping from code ranges to blocks.
This would be fine both for the profiler and for the "where am I running now"
indication.  Users might also be convinced that it was most interesting to
break at block starts, but I don't really know how easily people could develop
an understanding of basic blocks.

It could also be a bit tricky to map an arbitrary user-designated source
location to some "closest" source location actually in the debug info.
This problem probably exists to some degree even with a full source map, since
some forms will never appear as the source of any node.  It seems you might
have to negotiate with the user.  He would mouse something, and then you would
highlight some source form that has a common prefix (i.e. is a prefix of the
user path, or vice-versa.)  If they aren't happy with the result, they could
try something else.  In some cases, the designated path might be a prefix of
several paths.  This ambiguity might be resolved by picking the shortest path
or letting the user choose.

At the primitive level, I guess what this means is that the structure of source
locations (i.e. source paths) must be known, and the source=>code operation
should return a list of <source,code> pairs, rather than just a list of code
locations.  This allows the debugger to resolve the ambiguity however it wants.

I guess the formal definition of which source paths we would return is:
    All source paths in the debug info that have a maximal common prefix with
    the specified path.  i.e. if several paths have the complete specified path
    as a prefix, we return them all.  Otherwise, all paths with an equally
    large common prefix are returned: if the path with the most in common
    matches only the first three elements, then we return all paths that match
    in the first three elements.  As a degenerate case (which probably
    shouldn't happen), if there is no path with anything in common, then we
    return *all* of the paths.



In the DEBUG-SOURCE structure we may ultimately want a vector of the start
positions of each source form, since that would make it easier for the debugger
to locate the source.  It could just open the file, FILE-POSITION to the form,
do a READ, then loop down the source path.  Of course, it could read each form
starting from the beginning, but that might be too slow.


Do XEPs really need Debug-Functions?  The only time that we will commonly end
up in the debugger on an XEP is when an argument type check fails.  But I
suppose it would be nice to be able to print the arguments passed...


Note that assembler-level code motion such as pipeline reorganization can cause
problems with our PC maps.  The assembler needs to know that debug info markers
are different from real labels anyway, so I suppose it could inhibit motion
across debug markers conditional on policy.  It seems unworthwhile to remember
the node for each individual instruction.


For tracing block-compiled calls:
    Info about return value passing locations?
    Info about where all the returns are?

We definitely need the return-value passing locations for debug-return.  The
question is what the interface should be.  We don't really want to have a
visible debug-function-return-locations operation, since there are various
value passing conventions, and we want to paper over the differences.


Probably should be a compiler option to initialize stack frame to a special
uninitialized object (some random immediate type).  This would aid debugging,
and would also help GC problems.  For the latter reason especially, this should
be locally-turn-onable (off of policy?  the new debug-info quality?).


What about the interface between the evaluator and the debugger? (i.e. what
happens on an error, etc.)  Compiler error handling should be integrated with
run-time error handling.  Ideally the error messages should look the same.
Practically, in some cases the run-time errors will have less information.  But
the error should look the same to the debugger (or at least similar).



;;;; Debugger interface:

How does the debugger interface to the "evaluator" (where the evaluator means
all of native code, byte-code and interpreted IR1)?  It seems that it would be
much more straightforward to have a consistent user interface to debugging
all code representations if there was a uniform debugger interface to the
underlying stuff, and vice-versa.

Of course, some operations might not be supported by some representations, etc.
For example, fine-control stepping might not be available in native code.
In other cases, we might reduce an operation to the lowest common denominator,
for example fetching lexical variables by string and admitting the possibility
of ambiguous matches.
    Actually, it would probably be a good idea to store the package if we are
    going to allow variables to be closed over.

Some objects we would need:
Location:
	The constant information about the place where a value is stored,
        everything but which particular frame it is in.  Operations:
        location name, type, etc.
        location-value frame location (setf'able)
	monitor-location location function
            Function is called whenever location is set with the location,
            frame and old value.  If active values aren't supported, then we
            dummy the effect using breakpoints, in which case the change won't
            be noticed until the end of the block (and intermediate changes
            will be lost.)
debug info:
        All the debug information for a component.
Frame:
	frame-changed-locations frame => location*
            Return a list of the locations in frame that were changed since the
            last time this function was called.  Or something.  This is for
            displaying interesting state changes at breakpoints.
	save-frame-state frame => frame-state
	restore-frame-state frame frame-state
	    These operations allow the debugger to back up evaluation, modulo
	    side-effects and non-local control transfers.  This copies and
	    restores all variables, temporaries, etc, local to the frame, and
	    also the current PC and dynamic environment (current catch, etc.)

	    At the time of the save, the frame must be for the running function
	    (not waiting for a call to return.)  When we restore, the frame
	    becomes current again, effectively exiting from any frames on top.
	    (Of course, frame must not already be exited.)

Thread:
        Representation of which stack to use, etc.
Block:
        What successors the block has, what calls there are in the block.
        (Don't need to know where calls are as long as we know called function,
        since can breakpoint at the function.)  Whether code in this block is
        wildly out of order due to being the result of loop-invariant
        optimization, etc.  Operations:
        block-successors block => code-location*
        block-forms block => (source-location code-location)*
            Return the corresponding source locations and code locations for
            all forms (and form fragments) in the block.


Variable maps:

There are about five things that the debugger might want to know about a
variable:

    Name
	Although a lexical variable's name is "really" a symbol (package and
	all), in practice it doesn't seem worthwhile to require all the symbols
	for local variable names to be retained.  There is much less VM and GC
	overhead for a constant string than for a symbol.  (Also it is useful
	to be able to access gensyms in the debugger, even though they are
	theoretically ineffable).

    ID
	Which variable with the specified name is this?  It is possible to have
	multiple variables with the same name in a given function.  The ID is
	something that makes Name unique, probably a small integer.  When
	variables aren't unique, we could make this be part of the name, e.g.
	"FOO\#1", "FOO\#2".  But there are advantages to keeping this separate,
	since in many cases lifetime information can be used to disambiguate,
	making qualification unnecessary.

    SC
	When unboxed representations are in use, we must have type information
	to properly read and write a location.  We only need to know the
	SC for this, which would be amenable to a space-saving
	numeric encoding.

    Location
	Simple: the offset in SC.  (Actually, we need the save location too.)

    Lifetime
	In what parts of the program does this variable hold a meaningful
	value?  It seems prohibitive to record precise lifetime information,
	both in space and compiler effort, so we will have to settle for some
	sort of approximation.

	The finest granularity at which it is easy to determine liveness is the
	the block: we can regard the variable lifetime as the set of blocks
	that the variable is live in.  Of course, the variable may be dead (and
	thus contain meaningless garbage) during arbitrarily large portions of
	the block.

	Note that this subsumes the notion of which function a variable belongs
	to.  A given block is only in one function, so the function is
	implicit.


The variable map should represent this information space-efficiently and with
adequate computational efficiency.

The SC and ID can be represented as small integers.  Although the ID can in
principle be arbitrarily large, it should be <100 in practice.  The location
can be represented by just the offset (a moderately small integer), since the
SB is implicit in the SC.

The lifetime info can be represented either as a bit-vector indexed by block
numbers, or by a list of block numbers.  Which is more compact depends both on
the size of the component and on the number of blocks the variable is live in.
In the limit of large component size, the sparse representation will be more
compact, but it isn't clear where this crossover occurs.  Of course, it would
be possible to use both representations, choosing the more compact one on a
per-variable basis.  Another interesting special case is when the variable is
live in only one block: this may be common enough to be worth picking off,
although it is probably rarer for named variables than for TNs in general.

If we dump the type, then a normal list-style type descriptor is fine: the
space overhead is small, since the shareability is high.

We could probably save some space by cleverly representing the var-info as
parallel vectors of different types, but this would be more painful in use.
It seems better to just use a structure, encoding the unboxed fields in a
fixnum.  This way, we can pass around the structure in the debugger, perhaps
even exporting it from the the low-level debugger interface.

    XXX We need the save location too.  This probably means that we need two slots
    of bits, since we need the save offset and save SC.  Actually, we could let the
    save SC be implied by the normal SC, since at least currently, we always choose
    the same save SC for a given SC.  But even so, we probably can't fit all that
    stuff in one fixnum without squeezing a lot, so we might as well split and
    record both SCs.

    In a localized packing scheme, we would have to dump a different var-info
    whenever either the main location or the save location changes.  As a practical
    matter, the save location is less likely to change than the main location, and
    should never change without the main location changing.

    One can conceive of localized packing schemes that do saving as a special case
    of localized packing.  If we did this, then the concept of a save location
    might be eliminated, but this would require major changes in the IR2
    representation for call and/or lifetime info.  Probably we will want saving to
    continue to be somewhat magical.


How about:

(defstruct var-info
  ;;
  ;; This variable's name. (symbol-name of the symbol)
  (name nil :type simple-string)
  ;;
  ;; The SC, ID and offset, encoded as bit-fields.
  (bits nil :type fixnum)
  ;;
  ;; The set of blocks this variable is live in.  If a bit-vector, then it has
  ;; a 1 when indexed by the number of a block that it is live in.  If an
  ;; I-vector, then it lists the live block numbers.  If a fixnum, then that is
  ;; the number of the sole live block.
  (lifetime nil :type (or vector fixnum))
  ;;
  ;; The variable's type, represented as list-style type descriptor.
  type)

Then the debug-info holds a simple-vector of all the var-info structures for
that component.  We might as well make it sorted alphabetically by name, so
that we can binary-search to find the variable corresponding to a particular
name.

We need to be able to translate PCs to block numbers.  This can be done by an
I-Vector in the component that contains the start location of each block.  The
block number is the index at which we find the correct PC range.  This requires
that we use an emit-order block numbering distinct from the IR2-Block-Number,
but that isn't any big deal.  This seems space-expensive, but it isn't too bad,
since it would only be a fraction of the code size if the average block length
is a few words or more.

An advantage of our per-block lifetime representation is that it directly
supports keeping a variable in different locations when in different blocks,
i.e. multi-location packing.  We use a different var-info for each different
packing, since the SC and offset are potentially different.  The Name and ID
are the same, representing the fact that it is the same variable.  It is here
that the ID is most significant, since the debugger could otherwise make
same-name variables unique all by itself.



Stack parsing:

    XXX Probably not worth trying to make the stack parseable from the bottom up.
    There are too many complications when we start having variable sized stuff on
    the stack.  It seems more profitable to work on making top-down parsing robust.
    Since we are now planning to wire the bottom-up linkage info, scanning from the
    bottom to find the top frame shouldn't be too inefficient, even when there was
    a runaway recursion.  If we somehow jump into hyperspace, then the debugger may
    get confused, but we can debug this sort of low-level system lossage using
    ADB.


There are currently three relevant context pointers:
  * The PC.  The current PC is wired (implicit in the machine).  A saved
    PC (RETURN-PC) may be anywhere in the current frame.
  * The current stack context (CONT).  The current CONT is wired.  A saved
    CONT (OLD-CONT) may be anywhere in the current frame.
  * The current code object (ENV).  The current ENV is wired.  When saved,
    this is extra-difficult to locate, since it is saved by the caller, and is
    thus at an unknown offset in OLD-CONT, rather than anywhere in the current
    frame.

We must have all of these to parse the stack.

With the proposed Debug-Function, we parse the stack (starting at the top) like
this:
 1] Use ENV to locate the current Debug-Info
 2] Use the Debug-Info and PC to determine the current Debug-Function.
 3] Use the Debug-Function to find the OLD-CONT and RETURN-PC.
 4] Find the old ENV by searching up the stack for a saved code object
    containing the RETURN-PC.
 5] Assign old ENV to ENV, OLD-CONT to CONT, RETURN-PC to PC and goto 1.

If we changed the function representation so that the code and environment were
a single object, then the location of the old ENV would be simplified.  But we
still need to represent ENV as separate from PC, since interrupts and errors
can happen when the current PC isn't positioned at a valid return PC.

It seems like it might be a good idea to save OLD-CONT, RETURN-PC and ENV at
the beginning of the frame (before any stack arguments).  Then we wouldn't have
to search to locate ENV, and we also have a hope of parsing the stack even if
it is damaged.  As long as we can locate the start of some frame, we can trace
the stack above that frame.  We can recognize a probable frame start by
scanning the stack for a code object (presumably a saved ENV).

Probably we want some fairly general
mechanism for specifying that a TN should be considered to be live for the
duration of a specified environment.  It would be somewhat easier to specify
that the TN is live for all time, but this would become very space-inefficient
in large block compilations.

This mechanism could be quite useful for other debugger-related things.  For
example, when debuggability is important, we could make the TNs holding
arguments live for the entire environment.  This would guarantee that a
backtrace would always get the right value (modulo setqs).

Note that in this context, "environment" means the Environment structure (one
per non-let function).  At least according to current plans, even when we do
inter-routine register allocation, the different functions will have different
environments: we just "equate" the environments.  So the number of live
per-environment TNs is bounded by the size of a "function", and doesn't blow up
in block compilation.

The implementation is simple: per-environment TNs are flagged by the
:Environment kind.  :Environment TNs are treated the same as :Normal TNs by
everyone except for lifetime/conflict analysis.  An environment's TNs are also
stashed in a list in the IR2-Environment structure.  During during the conflict
analysis post-pass, we look at each block's environment, and make all the
environment's TNs always-live in that block.

We can implement the "fixed save location" concept needed for lazy frame
creation by allocating the save TNs as wired TNs at IR2 conversion time.  We
would use the new "environment lifetime" concept to specify the lifetimes of
the save locations.  There isn't any run-time overhead if we never get around
to using the save TNs.
    Pack would also have to notice TNs with pre-allocated
    save TNs, packing the original TN in the stack location if its FSC is the
    stack.


We want a standard (recognizable) format for an "escape" frame.  We must make
an escape frame whenever we start running another function without the current
function getting a chance to save its registers.  This may be due either to a
truly asynchronous event such as a software interrupt, or due to an "escape"
from a miscop.  An escape frame marks a brief conversion to a callee-saves
convention.

Whenever a miscop saves registers, it should make an escape frame.  This
ensures that the "current" register contents can always be located by the
debugger.  In this case, it may be desirable to be able to indicate that only
partial saving has been done.  For example, we don't want to have to save all
the FP registers just so that we can use a couple extra general registers.

When when the debugger see an escape frame, it knows that register values are
located in the escape frame's "register save" area, rather than in the normal
save locations.

It would be nice if there was a better solution to this internal error concept.
One problem is that it seems there is a substantial space penalty for emitting
all that error code, especially now that we don't share error code between
errors because we want to preserve the source context in the PC.  But this
probably isn't really all that bad when considered as a fraction of the code.
For example, the check part of a type check is 12 bytes, whereas the error part
is usually only 6.  In this case, we could never reduce the space overhead for
type checks by more than 1/3, thus the total code size reduction would be
small.  This will be made even less important when we do type check
optimizations to reduce the number of type checks.

Probably we should stick to the same general internal error mechanism, but make
it interact with the debugger better by allocating linkage registers and
allowing proceedable errors.  We could support shared error calls and
non-proceedable errors when space is more important than debuggability, but
this is probably more complexity than is worthwhile.

We jump or trap to a routine that saves the context (allocating at most the
return PC register).  We then encode the error and context in the code
immediately following the jump/trap.  (On the MIPS, the error code can be
encoded in the trap itself.)  The error arguments would be encoded as
SC-offsets relative to the saved context.  This could solve both the
arg-trashing problem and save space, since we could encode the SC-offsets more
tersely than the corresponding move instructions.
]#


;;;; Debug blocks.

(deftype location-kind ()
  '(member :unknown-return :known-return :internal-error :non-local-exit
	   :block-start :call-site :single-value-return :non-local-entry))

;;; The Location-Info structure holds the information what we need about
;;; locations which code generation decided were "interesting".
;;;
(defstruct (location-info
	    (:constructor make-location-info (kind label vop)))
  ;;
  ;; The kind of location noted.
  (kind nil :type location-kind)
  ;;
  ;; The label pointing to the interesting code location.
  (label nil :type (or label index null))
  ;;
  ;; The VOP that emitted this location (for node, save-set, ir2-block, etc.)
  (vop nil :type vop))

;;; NOTE-DEBUG-LOCATION  --  Interface
;;;
;;; Called during code generation in places where there is an "interesting"
;;; location: some place where we are likely to end up in the debugger, and
;;; thus want debug info.
;;;
(defun note-debug-location (vop label kind)
  (declare (type vop vop) (type (or label null) label)
	   (type location-kind kind))
  (let ((location (make-location-info kind label vop)))
    (setf (ir2-block-locations (vop-block vop))
	  (nconc (ir2-block-locations (vop-block vop))
		 (list location)))
    location))

;;; IR2-BLOCK-ENVIRONMENT  --  Interface
;;;
(proclaim '(inline ir2-block-environment))
(defun ir2-block-environment (2block)
  (declare (type ir2-block 2block))
  (block-environment (ir2-block-block 2block)))

;;; COMPUTE-LIVE-VARS  --  Internal
;;;
;;; Given a local conflicts vector and an IR2 block to represent the set of
;;; live TNs, and the Var-Locs hash-table representing the variables
;;; dumped, compute a bit-vector representing the set of live variables.
;;; If the TN is environment-live, we only mark it as live when it is in
;;; scope at Node.
;;;
(defun compute-live-vars (live node block var-locs vop)
  (declare (type ir2-block block) (type local-tn-bit-vector live)
	   (type hash-table var-locs) (type node node)
	   (type (or vop null) vop))
  (let ((res (make-array (logandc2 (+ (hash-table-count var-locs) 7) 7)
			 :element-type 'bit
			 :initial-element 0))
	(spilled (gethash vop
			  (ir2-component-spilled-vops
			   (component-info *compile-component*)))))
    (do-live-tns (tn live block)
      (let ((leaf (tn-leaf tn)))
	(when (and (lambda-var-p leaf)
		   (or (not (member (tn-kind tn)
				    '(:environment :debug-environment)))
		       (rassoc leaf (lexenv-variables (node-lexenv node))))
		   (or (null spilled)
		       (not (member tn spilled))))
	  (let ((num (gethash leaf var-locs)))
	    (when num
	      (setf (sbit res num) 1))))))
    res))

;;; The PC for the location most recently dumped.
;;;
(defvar *previous-location*)
(proclaim '(type index *previous-location*))

;;; DUMP-1-LOCATION  --  Internal
;;;
;;; Dump a compiled debug-location into *BYTE-BUFFER* that describes the
;;; code/source map and live info.  If true, VOP is the VOP associated with
;;; this location, for use in determining whether TNs are spilled.
;;;
(defun dump-1-location (node block kind tlf-num label live var-locs vop)
  (declare (type node node) (type ir2-block block)
	   (type local-tn-bit-vector live)
	   (type (or label index) label)
	   (type location-kind kind) (type (or index null) tlf-num)
	   (type hash-table var-locs) (type (or vop null) vop))

  (vector-push-extend
   (dpb (eposition kind compiled-code-location-kinds)
	compiled-code-location-kind-byte
	0)
   *byte-buffer*)

  (let ((loc (if (fixnump label) label (label-position label))))
    (write-var-integer (- loc *previous-location*) *byte-buffer*)
    (setq *previous-location* loc))

  (let ((path (node-source-path node)))
    (unless tlf-num
      (write-var-integer (source-path-tlf-number path) *byte-buffer*))
    (write-var-integer (source-path-form-number path) *byte-buffer*))

  (write-packed-bit-vector (compute-live-vars live node block var-locs vop)
			   *byte-buffer*)

  (undefined-value))

;;; DUMP-LOCATION-FROM-INFO  --  Internal
;;;
;;; Extract context info from a Location-Info structure and use it to dump
;;; a compiled code-location.
;;;
(defun dump-location-from-info (loc tlf-num var-locs)
  (declare (type location-info loc) (type (or index null) tlf-num)
	   (type hash-table var-locs))
  (let ((vop (location-info-vop loc)))
    (dump-1-location (vop-node vop)
		     (vop-block vop)
		     (location-info-kind loc)
		     tlf-num
		     (location-info-label loc)
		     (vop-save-set vop)
		     var-locs
		     vop))
  (undefined-value))

;;; FIND-TLF-NUMBER  --  Internal
;;;
;;; Scan all the blocks, determining if all locations are in the same TLF,
;;; and returing it or NIL.
;;;
(defun find-tlf-number (fun)
  (declare (type clambda fun))
  (let ((res (source-path-tlf-number (node-source-path (lambda-bind fun)))))
    (declare (type (or index null) res))
    (do-environment-ir2-blocks (2block (lambda-environment fun))
      (let ((block (ir2-block-block 2block)))
	(when (eq (block-info block) 2block)
	  (unless (eql (source-path-tlf-number
			(node-source-path
			 (continuation-next
			  (block-start block))))
		       res)
	    (setq res nil)))

	(dolist (loc (ir2-block-locations 2block))
	  (unless (eql (source-path-tlf-number
			(node-source-path
			 (vop-node (location-info-vop loc))))
		       res)
	    (setq res nil)))))
    res))

;;; DUMP-BLOCK-LOCATIONS  --  Internal
;;;
;;; Dump out the number of locations and the locations for Block.
;;;
(defun dump-block-locations (block locations tlf-num var-locs)
  (declare (type cblock block) (list locations))
  (if (and locations
	   (eq (location-info-kind (first locations))
	       :non-local-entry))
      (write-var-integer (length locations) *byte-buffer*)
      (let ((2block (block-info block)))
	(write-var-integer (+ (length locations) 1) *byte-buffer*)
	(dump-1-location (continuation-next (block-start block))
			 2block :block-start tlf-num
			 (ir2-block-%label 2block)
			 (ir2-block-live-out 2block)
			 var-locs
			 nil)))
  (dolist (loc locations)
    (dump-location-from-info loc tlf-num var-locs))
  (undefined-value))

;;; DUMP-BLOCK-SUCCESSORS  --  Internal
;;;
;;; Dump the successors of Block, being careful not to fly into space on
;;; weird successors.
;;;
(defun dump-block-successors (block env)
  (declare (type cblock block) (type environment env))
  (let* ((tail (component-tail (block-component block)))
	 (succ (block-succ block))
	 (valid-succ
	  (if (and succ
		   (or (eq (car succ) tail)
		       (not (eq (block-environment (car succ)) env))))
	      ()
	      succ)))
    (vector-push-extend
     (dpb (length valid-succ) compiled-debug-block-nsucc-byte 0)
     *byte-buffer*)
    (let ((base (block-number
		 (node-block
		  (lambda-bind (environment-function env))))))
      (dolist (b valid-succ)
	(write-var-integer
	 (the index (- (block-number b) base))
	 *byte-buffer*))))
  (undefined-value))

;;; COMPUTE-DEBUG-BLOCKS  --  Internal
;;;
;;; Return a vector and an integer (or null) suitable for use as the BLOCKS
;;; and TLF-NUMBER in Fun's debug-function.  This requires two passes to
;;; compute:
;;; -- Scan all blocks, dumping the header and successors followed by all the
;;;    non-elsewhere locations.
;;; -- Dump the elsewhere block header and all the elsewhere locations (if
;;;    any.)
;;;
(defun compute-debug-blocks (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (setf (fill-pointer *byte-buffer*) 0)
  (let ((*previous-location* 0)
	(tlf-num (find-tlf-number fun))
	(env (lambda-environment fun))
	(prev-locs nil)
	(prev-block nil))
    (collect ((elsewhere))
      (do-environment-ir2-blocks (2block env)
	(let ((block (ir2-block-block 2block)))
	  (when (eq (block-info block) 2block)
	    (when prev-block
	      (dump-block-locations prev-block prev-locs tlf-num var-locs))
	    (setq prev-block block  prev-locs ())
	    (dump-block-successors block env)))

	(collect ((here prev-locs))
	  (dolist (loc (ir2-block-locations 2block))
	    (if (label-elsewhere-p (location-info-label loc))
		(elsewhere loc)
		(here loc)))
	  (setq prev-locs (here))))

      (dump-block-locations prev-block prev-locs tlf-num var-locs)

      (when (elsewhere)
	(vector-push-extend compiled-debug-block-elsewhere-p *byte-buffer*)
	(write-var-integer (length (elsewhere)) *byte-buffer*)
	(dolist (loc (elsewhere))
	  (dump-location-from-info loc tlf-num var-locs))))

    (values (copy-seq *byte-buffer*) tlf-num)))


;;; DEBUG-SOURCE-FOR-INFO  --  Interface
;;;
;;; Return a list of DEBUG-SOURCE structures containing information derived
;;; from Info.  Unless :BYTE-COMPILE T was specified, we always dump the
;;; Start-Positions, since it is too hard figure out whether we need them
;;; or not.
;;;
(defun debug-source-for-info (info)
  (declare (type source-info info))
  (assert (fi (source-info-current-file info)))
  (mapcar #'(lambda (x)
	      (let ((res (make-debug-source
			  :from :file
			  :comment (file-info-comment x)
			  :created (file-info-write-date x)
			  :compiled (source-info-start-time info)
			  :source-root (file-info-source-root x)
			  :start-positions
			  (unless (eq *byte-compile* 't)
			    (coerce-to-smallest-eltype
			     (file-info-positions x)))))
		    (name (file-info-name x)))
		(etypecase name
		  ((member :stream :lisp)
		   (setf (debug-source-from res) name)
		   (setf (debug-source-name res)
			 (coerce (file-info-forms x) 'simple-vector)))
		  (pathname
		   (let* ((untruename (file-info-untruename x))
			  (dir (pathname-directory untruename)))
		     (setf (debug-source-name res)
			   (namestring
			    (if (and dir (eq (first dir) :absolute))
				untruename
				name))))))
		res))
	  (source-info-files info)))

;;; COERCE-TO-SMALLEST-ELTYPE  --  Internal
;;;
;;; Given an arbirtary sequence, coerce it to an unsigned vector if
;;; possible.
;;;
(defun coerce-to-smallest-eltype (seq)
  (declare (type sequence seq))
  (let ((max 0))
    (declare (type (or index null) max))
    (macrolet ((frob ()
		 '(if (and (typep val 'index) max)
		      (when (> val max)
			(setq max val))
		      (setq max nil))))
      (if (listp seq)
	  (dolist (val seq)
	    (frob))
	  (dotimes (i (length seq))
	    (let ((val (aref seq i)))
	      (frob)))))

    (if max
	(coerce seq `(simple-array (integer 0 ,max) (*)))
	(coerce seq 'simple-vector))))


;;;; Variables.

;;; TN-SC-OFFSET  --  Internal
;;;
;;; Return a SC-OFFSET describing TN's location.
;;;
(defun tn-sc-offset (tn)
  (declare (type tn tn))
  (make-sc-offset (sc-number (tn-sc tn))
		  (tn-offset tn)))

;;; DUMP-1-VARIABLE  --  Internal
;;;
;;; Dump info to represent Var's location being TN.  ID is an integer that
;;; makes Var's name unique in the function.  Buffer is the vector we stick
;;; the result in.  If Minimal is true, we suppress name dumping, and set
;;; the minimal flag.
;;;
;;; The debug-variable is only marked as always-live if the TN is
;;; environment live and is an argument.  If a :debug-environment TN, then
;;; we also exclude set variables, since the variable is not guranteed to
;;; be live everywhere in that case.
;;;
(defun dump-1-variable (fun var tn id minimal buffer)
  (declare (type lambda-var var) (type (or tn null) tn) (type index id)
	   (type clambda fun))
  (let* ((name (leaf-name var))
	 (package (symbol-package name))
	 (package-p (and package (not (eq package *package*))))
	 (save-tn (and tn (tn-save-tn tn)))
	 (kind (and tn (tn-kind tn)))
	 (flags 0))
    (declare (type index flags))
    (cond (minimal
	   (setq flags (logior flags compiled-debug-variable-minimal-p))
	   (unless tn
	     (setq flags (logior flags compiled-debug-variable-deleted-p))))
	  (t
	   (unless package
	     (setq flags (logior flags compiled-debug-variable-uninterned)))
	   (when package-p
	     (setq flags (logior flags compiled-debug-variable-packaged)))))
    (when (and (or (eq kind :environment)
		   (and (eq kind :debug-environment)
			(null (basic-var-sets var))))
	       (not (gethash tn (ir2-component-spilled-tns
				 (component-info *compile-component*))))
	       (eq (lambda-var-home var) fun))
      (setq flags (logior flags compiled-debug-variable-environment-live)))
    (when save-tn
      (setq flags (logior flags compiled-debug-variable-save-loc-p)))
    (unless (or (zerop id) minimal)
      (setq flags (logior flags compiled-debug-variable-id-p)))
    (vector-push-extend flags buffer)
    (unless minimal
      (write-var-string (symbol-name name) buffer)
      (when package-p
	(write-var-string (package-name package) buffer))
      (unless (zerop id)
	(write-var-integer id buffer)))
    (if tn
	(write-var-integer (tn-sc-offset tn) buffer)
	(assert minimal))
    (when save-tn
      (write-var-integer (tn-sc-offset save-tn) buffer)))
  (undefined-value))

;;; COMPUTE-VARIABLES  --  Internal
;;;
;;; Return a vector suitable for use as the DEBUG-FUNCTION-VARIABLES of
;;; Fun.  Level is the current DEBUG-INFO quality.  Var-Locs is a hashtable
;;; in which we enter the translation from LAMBDA-VARS to the relative
;;; position of that variable's location in the resulting vector.
;;;
(defun compute-variables (fun level var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((vars))
    (labels ((frob-leaf (leaf tn gensym-p)
	       (let ((name (leaf-name leaf)))
		 (when (and name (leaf-refs leaf) (tn-offset tn)
			    (or gensym-p (symbol-package name)))
		   (vars (cons leaf tn)))))
	     (frob-lambda (x gensym-p)
	       (dolist (leaf (lambda-vars x))
		 (frob-leaf leaf (leaf-info leaf) gensym-p))))
      (frob-lambda fun t)
      (when (>= level 2)
	(dolist (x (ir2-environment-environment
		    (environment-info (lambda-environment fun))))
	  (let ((thing (car x)))
	    (when (lambda-var-p thing)
	      (frob-leaf thing (cdr x) (= level 3)))))

	(dolist (let (lambda-lets fun))
	  (frob-lambda let (= level 3)))))

    (setf (fill-pointer *byte-buffer*) 0)
    (let ((sorted (sort (vars) #'string<
			:key #'(lambda (x)
				 (symbol-name (leaf-name (car x))))))
	  (prev-name nil)
	  (id 0)
	  (i 0))
      (declare (type (or simple-string null) prev-name)
	       (type index id i))
      (dolist (x sorted)
	(let* ((var (car x))
	       (name (symbol-name (leaf-name var))))
	  (cond ((and prev-name (string= prev-name name))
		 (incf id))
		(t
		 (setq id 0  prev-name name)))
	  (dump-1-variable fun var (cdr x) id nil *byte-buffer*)
	  (setf (gethash var var-locs) i))
	(incf i)))

    (copy-seq *byte-buffer*)))

;;; COMPUTE-MINIMAL-VARIABLES  --  Internal
;;;
;;; Dump out the arguments to Fun in the minimal variable format.
;;;
(defun compute-minimal-variables (fun)
  (declare (type clambda fun))
  (setf (fill-pointer *byte-buffer*) 0)
  (dolist (var (lambda-vars fun))
    (dump-1-variable fun var (leaf-info var) 0 t *byte-buffer*))
  (copy-seq *byte-buffer*))

;;; DEBUG-LOCATION-FOR  --  Internal
;;;
;;; Return Var's relative position in the function's variables (determined
;;; from the Var-Locs hashtable.)  If Var is deleted, the return DELETED.
;;;
(defun debug-location-for (var var-locs)
  (declare (type lambda-var var) (type hash-table var-locs))
  (let ((res (gethash var var-locs)))
    (cond (res)
	  (t
	   (assert (or (null (leaf-refs var))
		       (not (tn-offset (leaf-info var)))))
	   'deleted))))


;;;; Arguments/returns.

;;; COMPUTE-ARGUMENTS  --  Internal
;;;
;;; Return a vector to be used as the COMPILED-DEBUG-FUNCTION-ARGUMENTS for
;;; Fun.  If fun is the MAIN-ENTRY for an optional dispatch, then look at
;;; the ARGLIST to determine the syntax, otherwise pretend all arguments
;;; are fixed.
;;;
;;; ### This assumption breaks down in EPs other than the main-entry, since
;;; they may or may not have supplied-p vars, etc.
;;;
(defun compute-arguments (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((res))
    (let ((od (lambda-optional-dispatch fun)))
      (if (and od (eq (optional-dispatch-main-entry od) fun))
	  (let ((actual-vars (lambda-vars fun))
		(saw-optional nil))
	    (dolist (arg (optional-dispatch-arglist od))
	      (let ((info (lambda-var-arg-info arg))
		    (actual (pop actual-vars)))
		(cond (info
		       (case (arg-info-kind info)
			 (:keyword
			  (res (arg-info-keyword info)))
			 (:rest
			  (res 'rest-arg))
			 (:more-context
			  (res 'more-arg))
			 (:optional
			  (unless saw-optional
			    (res 'optional-args)
			    (setq saw-optional t))))
		       (res (debug-location-for actual var-locs))
		       (when (arg-info-supplied-p info)
			 (res 'supplied-p)
			 (res (debug-location-for (pop actual-vars) var-locs))))
		      (t
		       (res (debug-location-for actual var-locs)))))))
	  (dolist (var (lambda-vars fun))
	    (res (debug-location-for var var-locs)))))

    (coerce-to-smallest-eltype (res))))

;;; COMPUTE-DEBUG-RETURNS  --  Internal
;;;
;;; Return a vector of SC offsets describing Fun's return locations.  (Must
;;; be known values return...)
;;;
(defun compute-debug-returns (fun)
  (coerce-to-smallest-eltype
   (mapcar #'(lambda (loc)
	       (tn-sc-offset loc))
	   (return-info-locations (tail-set-info (lambda-tail-set fun))))))


;;;; Debug functions.

;;; DFUN-FROM-FUN  --  Internal
;;;
;;; Return a C-D-F structure with all the mandatory slots filled in.
;;;
(defun dfun-from-fun (fun)
  (declare (type clambda fun))
  (let* ((2env (environment-info (lambda-environment fun)))
	 (dispatch (lambda-optional-dispatch fun))
	 (main-p (and dispatch
		      (eq fun (optional-dispatch-main-entry dispatch)))))
    (make-compiled-debug-function
     :name (cond ((leaf-name fun))
		 ((let ((ef (functional-entry-function
			     fun)))
		    (and ef (leaf-name ef))))
		 ((and main-p (leaf-name dispatch)))
		 (t
		  (component-name
		   (block-component (node-block (lambda-bind fun))))))
     :kind (if main-p nil (functional-kind fun))
     :return-pc (tn-sc-offset (ir2-environment-return-pc 2env))
     :old-fp (tn-sc-offset (ir2-environment-old-fp 2env))
     :start-pc (label-position (ir2-environment-environment-start 2env))
     :elsewhere-pc (label-position (ir2-environment-elsewhere-start 2env)))))

;;; COMPUTE-1-DEBUG-FUNCTION  --  Internal
;;;
;;; Return a complete COMPILED-DEBUG-FUNCTION structure for Fun.  This
;;; involves determining the DEBUG-INFO level and filling in optional slots
;;; as appropriate.
;;;
(defun compute-1-debug-function (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (let* ((dfun (dfun-from-fun fun))
	 (actual-level
	  (cookie-debug (lexenv-cookie (node-lexenv (lambda-bind fun)))))
	 (level (if *collect-dynamic-statistics*
		    (max actual-level 2)
		    actual-level)))
    (cond ((zerop level))
	  ((and (<= level 1)
		(let ((od (lambda-optional-dispatch fun)))
		  (or (not od)
		      (not (eq (optional-dispatch-main-entry od) fun)))))
	   (setf (compiled-debug-function-variables dfun)
		 (compute-minimal-variables fun))
	   (setf (compiled-debug-function-arguments dfun) :minimal))
	  (t
	   (setf (compiled-debug-function-variables dfun)
		 (compute-variables fun level var-locs))
	   (setf (compiled-debug-function-arguments dfun)
		 (compute-arguments fun var-locs))))

    (when (>= level 2)
      (multiple-value-bind (blocks tlf-num)
			   (compute-debug-blocks fun var-locs)
	(setf (compiled-debug-function-tlf-number dfun) tlf-num)
	(setf (compiled-debug-function-blocks dfun) blocks)))

    (if (external-entry-point-p fun)
	(setf (compiled-debug-function-returns dfun) :standard)
	(let ((info (tail-set-info (lambda-tail-set fun))))
	  (when info
	    (cond ((eq (return-info-kind info) :unknown)
		   (setf (compiled-debug-function-returns dfun)
			 :standard))
		  ((/= level 0)
		   (setf (compiled-debug-function-returns dfun)
			 (compute-debug-returns fun)))))))
    dfun))


;;;; Minimal debug functions.

;;; DEBUG-FUNCTION-MINIMAL-P  --  Internal
;;;
;;; Return true if Dfun can be represented as a minimal debug function.
;;; Dfun is a cons (<start offset> . C-D-F).
;;;
(defun debug-function-minimal-p (dfun)
  (declare (type cons dfun))
  (let ((dfun (cdr dfun)))
    (and (member (compiled-debug-function-arguments dfun) '(:minimal nil))
	 (null (compiled-debug-function-blocks dfun)))))

;;; DUMP-1-MINIMAL-DFUN  --  Internal
;;;
;;; Dump a packed binary representation of a Dfun into *byte-buffer*.
;;; Prev-Start and Start are the byte offsets in the code where the
;;; previous function started and where this one starts.  Prev-Elsewhere is
;;; the previous function's elsewhere PC.
;;;
(defun dump-1-minimal-dfun (dfun prev-start start prev-elsewhere)
  (declare (type compiled-debug-function dfun)
	   (type index prev-start start prev-elsewhere))
  (let* ((name (compiled-debug-function-name dfun))
	 (setf-p (and (consp name) (eq (car name) 'setf)
		      (consp (cdr name)) (symbolp (cadr name))))
	 (base-name (if setf-p (cadr name) name))
	 (pkg (when (symbolp base-name)
		(symbol-package base-name)))
	 (name-rep
	  (cond ((stringp base-name)
		 minimal-debug-function-name-component)
		((not pkg)
		 minimal-debug-function-name-uninterned)
		((eq pkg *package*)
		 minimal-debug-function-name-symbol)
		(t
		 minimal-debug-function-name-packaged))))
    (assert (or (atom name) setf-p))
    (let ((options 0))
      (setf (ldb minimal-debug-function-name-style-byte options) name-rep)
      (setf (ldb minimal-debug-function-kind-byte options)
	    (eposition (compiled-debug-function-kind dfun)
		      minimal-debug-function-kinds))
      (setf (ldb minimal-debug-function-returns-byte options)
	    (etypecase (compiled-debug-function-returns dfun)
	      ((member :standard) minimal-debug-function-returns-standard)
	      ((member :fixed) minimal-debug-function-returns-fixed)
	      (vector minimal-debug-function-returns-specified)))
      (vector-push-extend options *byte-buffer*))

    (let ((flags 0))
      (when setf-p
	(setq flags (logior flags minimal-debug-function-setf-bit)))
      (when (compiled-debug-function-nfp dfun)
	(setq flags (logior flags minimal-debug-function-nfp-bit)))
      (when (compiled-debug-function-variables dfun)
	(setq flags (logior flags minimal-debug-function-variables-bit)))
      (vector-push-extend flags *byte-buffer*))

    (when (eql name-rep minimal-debug-function-name-packaged)
      (write-var-string (package-name pkg) *byte-buffer*))
    (unless (stringp base-name)
      (write-var-string (symbol-name base-name) *byte-buffer*))

    (let ((vars (compiled-debug-function-variables dfun)))
      (when vars
	(let ((len (length vars)))
	  (write-var-integer len *byte-buffer*)
	  (dotimes (i len)
	    (vector-push-extend (aref vars i) *byte-buffer*)))))

    (let ((returns (compiled-debug-function-returns dfun)))
      (when (vectorp returns)
	(let ((len (length returns)))
	  (write-var-integer len *byte-buffer*)
	  (dotimes (i len)
	    (write-var-integer (aref returns i) *byte-buffer*)))))

    (write-var-integer (compiled-debug-function-return-pc dfun)
		       *byte-buffer*)
    (write-var-integer (compiled-debug-function-old-fp dfun)
		       *byte-buffer*)
    (when (compiled-debug-function-nfp dfun)
      (write-var-integer (compiled-debug-function-nfp dfun)
			 *byte-buffer*))
    (write-var-integer (- start prev-start) *byte-buffer*)
    (write-var-integer (- (compiled-debug-function-start-pc dfun) start)
		       *byte-buffer*)
    (write-var-integer (- (compiled-debug-function-elsewhere-pc dfun)
			  prev-elsewhere)
		       *byte-buffer*)))

;;; COMPUTE-MINIMAL-DEBUG-FUNCTIONS  --  Internal
;;;
;;; Return a byte-vector holding all the debug functions for a component in
;;; the packed binary minimal-debug-function format.
;;;
(defun compute-minimal-debug-functions (dfuns)
  (declare (list dfuns))
  (setf (fill-pointer *byte-buffer*) 0)
  (let ((prev-start 0)
	(prev-elsewhere 0))
    (dolist (dfun dfuns)
      (let ((start (car dfun))
	    (elsewhere (compiled-debug-function-elsewhere-pc (cdr dfun))))
	(dump-1-minimal-dfun (cdr dfun) prev-start start prev-elsewhere)
	(setq prev-start start  prev-elsewhere elsewhere))))
  (copy-seq *byte-buffer*))


;;;; Full component dumping.

;;; COMPUTE-DEBUG-FUNCTION-MAP  --  Internal
;;;
;;; Compute the full form (simple-vector) function map.
;;;
(defun compute-debug-function-map (sorted)
  (declare (list sorted))
  (let* ((len (1- (* (length sorted) 2)))
	 (funs-vec (make-array len)))
    (do ((i -1 (+ i 2))
	 (sorted sorted (cdr sorted)))
	((= i len))
      (declare (fixnum i))
      (let ((dfun (car sorted)))
	(unless (minusp i)
	  (setf (svref funs-vec i) (car dfun)))
	(setf (svref funs-vec (1+ i)) (cdr dfun))))
    funs-vec))

;;; DEBUG-INFO-FOR-COMPONENT  --  Interface
;;;
;;; Return a debug-info structure describing component.  This has to be
;;; called after assembly so that source map information is available.
;;;
(defun debug-info-for-component (component)
  (declare (type component component))
  (let ((res (make-compiled-debug-info :name (component-name component)
				       :package (package-name *package*))))
    (collect ((dfuns))
      (let ((var-locs (make-hash-table :test #'eq))
	    (*byte-buffer*
	     (make-array 10 :element-type '(unsigned-byte 8)
			 :fill-pointer 0  :adjustable t)))
	(dolist (fun (component-lambdas component))
	  (clrhash var-locs)
	  (dfuns (cons (label-position
			(block-label (node-block (lambda-bind fun))))
		       (compute-1-debug-function fun var-locs))))

	(let ((sorted (sort (dfuns) #'< :key #'car)))
	  (setf (compiled-debug-info-function-map res)
		(if (every #'debug-function-minimal-p sorted)
		    (compute-minimal-debug-functions sorted)
		    (compute-debug-function-map sorted))))))

    res))
