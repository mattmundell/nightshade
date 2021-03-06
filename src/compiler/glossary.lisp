(in-package "C")

#[ Compiler Glossary

FIX could this be part of a dictionary?

% Note: in an entry, any word that is also defined should be \it
% should entries have page references as well?

\begin{description}
\item[assert (a type)]
In the compiler, all type checking is done via a general type assertion
mechanism.  Explicit declarations and implicit assertions (e.g. the arg to
+ is a number) are recorded in the front-end (implicit continuation)
representation.  Type assertions (and thus type-checking) are "unbundled"
from the operations that are affected by the assertion.  This has two major
advantages:
\begin{itemize}
\item Code that implements operations need not concern itself with checking
operand types.

\item Run-time type checks can be eliminated when the compiler can prove that
the assertion will always be satisfied.
\end{itemize}
See also {\it restrict}.

\item[back end] The back end is the part of the compiler that operates on the
{\it virtual machine} intermediate representation.  Also included are the
compiler phases involved in the conversion from the {\it front end}
representation (or {\it ICR}).

\item[bind node] This is a node type the that marks the start of a {\it lambda}
body in {\it ICR}.  This serves as a placeholder for environment manipulation
code.

\item[IR1] The first intermediate representation, also known as {\it ICR}, or
the Implicit Continuation Represenation.

\item[IR2] The second intermediate representation, also known as {\it VMR}, or
the Virtual Machine Representation.

\item[basic block] A basic block (or simply "block") has the pretty much the
usual meaning of representing a straight-line sequence of code.  However, the
code sequence ultimately generated for a block might contain internal branches
that were hidden inside the implementation of a particular operation.  The type
of a block is actually {\tt cblock}.  The {\tt block-info} slot holds an
{\tt VMR-block} containing backend information.

\item[block compilation] Block compilation is a term commonly used to describe
the compile-time resolution of function names.  This enables many
optimizations.

\item[call graph]
Each node in the call graph is a function (represented by a {\it flow graph}.)
The arcs in the call graph represent a possible call from one function to
another.  See also {\it tail set}.

\item[cleanup]
A cleanup is the part of the implicit continuation representation that
retains information scoping relationships.  For indefinite extent bindings
(variables and functions), we can abandon scoping information after ICR
conversion, recovering the lifetime information using flow analysis.  But
dynamic bindings (special values, catch, unwind protect, etc.) must be
removed at a precise time (whenever the scope is exited.)  Cleanup
structures form a hierarchy that represents the static nesting of dynamic
binding structures.  When the compiler does a control transfer, it can use
the cleanup information to determine what cleanup code needs to be emitted.

\item[closure variable]
A closure variable is any lexical variable that has references outside of
its {\it home environment}.  See also {\it indirect value cell}.

\item[closed continuation] A closed continuation represents a {\tt tagbody} tag
or {\tt block} name that is closed over.  These two cases are mostly
indistinguishable in {\it ICR}.

\item[home] Home is a term used to describe various back-pointers.  A lambda
variable's "home" is the lambda that the variable belongs to.  A lambda's "home
environment" is the environment in which that lambda's variables are allocated.

\item[indirect value cell]
Any closure variable that has assignments ({\tt setq}s) will be allocated in an
indirect value cell.  This is necessary to ensure that all references to
the variable will see assigned values, since the compiler normally freely
copies values when creating a closure.

\item[set variable] Any variable that is assigned to is called a "set
variable".  Several optimizations must special-case set variables, and set
closure variables must have an {\it indirect value cell}.

\item[code generator] The code generator for a {\it VOP} is a potentially
arbitrary list code fragment which is responsible for emitting assembly code to
implement that VOP.

\item[constant pool] The part of a compiled code object that holds pointers to
non-immediate constants.

\item[constant TN]
A constant TN is the {\it VMR} of a compile-time constant value.  A
constant may be immediate, or may be allocated in the {\it constant pool}.

\item[constant leaf]
A constant {\it leaf} is the {\it ICR} of a compile-time constant value.

\item[combination]
A combination {\it node} is the {\it ICR} of any fixed-argument function
call (not {\tt apply} or {\tt multiple-value-call}.)

\item[top-level component]
A top-level component is any component whose only entry points are top-level
lambdas.

\item[top-level lambda]
A top-level lambda represents the execution of the outermost form on which
the compiler was invoked.  In the case of {\tt compile-file}, this is often a
truly top-level form in the source file, but the compiler can recursively
descend into some forms ({\tt eval-when}, etc.) breaking them into separate
compilations.

\item[component] A component is basically a sequence of blocks.  Each component
is compiled into a separate code object.  With {\it block compilation} or {\it
local functions}, a component will contain the code for more than one function.
This is called a component because it represents a connected portion of the
call graph.  Normally the blocks are in depth-first order ({\it DFO}).

\item[component, initial] During ICR conversion, blocks are temporarily
assigned to initial components.  The "flow graph canonicalization" phase
determines the true component structure.

\item[component, head and tail]
The head and tail of a component are dummy blocks that mark the start and
end of the {\it DFO} sequence.  The component head and tail double as the root
and finish node of the component's flow graph.

\item[local function (call)]
A local function call is a call to a function known at compile time to be
in the same {\it component}.  Local call allows compile time resolution of the
target address and calling conventions.  See {\it block compilation}.

\item[conflict (of TNs, set)]
Register allocation terminology.  Two TNs conflict if they could ever be
live simultaneously.  The conflict set of a TN is all TNs that it conflicts
with.

\item[continuation]
The ICR data structure which represents both:
\begin{itemize}
\item The receiving of a value (or multiple values), and

\item A control location in the flow graph.
\end{itemize}
In the Implicit Continuation Representation, the environment is implicit in the
continuation's BLOCK (hence the name.)  The ICR continuation is very similar to
a CPS continuation in its use, but its representation doesn't much resemble (is
not interchangeable with) a lambda.

\item[cont] A slot in the {\it node} holding the {\it continuation} which
receives the node's value(s).  Unless the node ends a {\it block}, this also
implicitly indicates which node should be evaluated next.

\item[cost] Approximations of the run-time costs of operations are widely used
in the back end.  By convention, the unit is generally machine cycles, but the
values are only used for comparison between alternatives.  For example, the
VOP cost is used to determine the preferred order in which to try possible
implementations.

\item[CSP, CFP] See {\it control stack pointer} and {\it control frame
pointer}.

\item[Control stack] The main call stack, which holds function stack frames.
All words on the control stack are tagged {\it descriptors}.  In all ports done
so far, the control stack grows from low memory to high memory.  The most
recent call frames are considered to be ``on top'' of earlier call frames.

\item[Control stack pointer] The allocation pointer for the {\it control
stack}.  Generally this points to the first free word at the top of the stack.

\item[Control frame pointer] The pointer to the base of the {\it control stack}
frame for a particular function invocation.  The CFP for the running function
must be in a register.

\item[Number stack] The auxiliary stack used to hold any {\it non-descriptor}
(untagged) objects.  This is generally the same as the C call stack, and thus
typically grows down.

\item[Number stack pointer] The allocation pointer for the {\it number stack}.
This is typically the C stack pointer, and is thus kept in a register.

\item[NSP, NFP] See {\it number stack pointer}, {\it number frame pointer}.

\item[Number frame pointer] The pointer to the base of the {\it number stack}
frame for a particular function invocation.  Functions that don't use the
number stack won't have an NFP, but if an NFP is allocated, it is always
allocated in a particular register.  If there is no variable-size data on the
number stack, then the NFP will generally be identical to the NSP.

\item[Lisp return address] The name of the {\it descriptor} encoding the
"return pc" for a function call.

\item[LRA] See {\it lisp return address}.  Also, the name of the register where
the LRA is passed.


\item[Code pointer] A pointer to the header of a code object.  The code pointer
for the currently running function is stored in the {\tt code} register.

\item[Interior pointer] A pointer into the inside of some heap-allocated
object.  Interior pointers confuse the garbage collector, so their use is
highly constrained.  Typically there is a single register dedicated to holding
interior pointers.

\item[dest]
A slot in the {\it continuation} which points the the node that receives this
value.  Null if this value is not received by anyone.

\item[DFN, DFO] See {\it Depth First Number}, {\it Depth First Order}.

\item[Depth first number] Blocks are numbered according to their appearance in
the depth-first ordering (the {\tt block-number} slot.)  The numbering actually
increases from the component tail, so earlier blocks have larger numbers.

\item[Depth first order] This is a linearization of the flow graph, obtained by
a depth-first walk.  Iterative flow analysis algorithms work better when blocks
are processed in DFO (or reverse DFO.)


\item[Object] In low-level design discussions, an object is one of the
following:
\begin{itemize}
\item a single word containing immediate data (characters, fixnums, etc)
\item a single word pointing to an object (structures, conses, etc.)
\end{itemize}
These are tagged with three low-tag bits as described in the section
\ref{tagging} This is synonymous with {\it descriptor}.
In other parts of the documentation, may be used more loosely to refer to a
{\it lisp object}.

\item[Lisp object]
A Lisp object is a high-level object discussed as a data type in the Common
Lisp definition.

\item[Data-block]
A data-block is a dual-word aligned block of memory that either manifests a
Lisp object (vectors, code, symbols, etc.) or helps manage a Lisp object on
the heap (array header, function header, etc.).

\item[Descriptor]
A descriptor is a tagged, single-word object.  It either contains immediate
data or a pointer to data.  This is synonymous with {\it object}.  Storage
locations that must contain descriptors are referred to as descriptor
locations.

\item[Pointer descriptor]
A descriptor that points to a {\it data block} in memory (i.e. not an immediate
object.)

\item[Immediate descriptor]
A descriptor that encodes the object value in the descriptor itself; used for
characters, fixnums, etc.

\item[Word]
A word is a 32-bit quantity.

\item[Non-descriptor]
Any chunk of bits that isn't a valid tagged descriptor.  For example, a
double-float on the number stack.  Storage locations that are not scanned by
the garbage collector (and thus cannot contain {\it pointer descriptors}) are
called non-descriptor locations.  {\it Immediate descriptors} can be stored in
non-descriptor locations.


\item[Entry point] An entry point is a function that may be subject to
``unpredictable'' control transfers.  All entry points are linked to the root
of the flow graph (the component head.)  The only functions that aren't entry
points are {\it let} functions.  When complex lambda-list syntax is used,
multiple entry points may be created for a single lisp-level function.
See {\it external entry point}.

\item[External entry point] A function that serves as a ``trampoline'' to
intercept function calls coming in from outside of the component.  The XEP does
argument syntax and type checking, and may also translate the arguments and
return values for a locally specialized calling calling convention.

\item[XEP] An {\it external entry point}.

\item[lexical environment] A lexical environment is a structure that is used
during VMR conversion to represent all lexically scoped bindings (variables,
functions, declarations, etc.)  Each {\tt node} is annotated with its lexical
environment, primarily for use by the debugger and other user interfaces.  This
structure is also the environment object passed to {\tt macroexpand}.

\item[environment] The environment is part of the ICR, created during
environment analysis.  Environment analysis apportions code to disjoint
environments, with all code in the same environment sharing the same stack
frame.  Each environment has a ``{\it real}'' function that allocates it, and
some collection {\tt let} functions.   Although environment analysis is the
last ICR phase, in earlier phases, code is sometimes said to be ``in the
same/different environment(s)''.  This means that the code will definitely be
in the same environment (because it is in the same real function), or that is
might not be in the same environment, because it is not in the same function.

\item[fixup]  Some sort of back-patching annotation.  The main sort encountered
are load-time {\it assembler fixups}, which are a linkage annotation mechanism.

\item[flow graph] A flow graph is a directed graph of basic blocks, where each
arc represents a possible control transfer.  The flow graph is the basic data
structure used to represent code, and provides direct support for data flow
analysis.  See component and ICR.

\item[foldable] An attribute of {\it known functions}.  A function is foldable
if calls may be constant folded whenever the arguments are compile-time
constant.  Generally this means that it is a pure function with no side
effects.


FSC
full call
function attribute
function
	"real" (allocates environment)
	meaning function-entry
	more vague (any lambda?)
funny function
GEN (kill and...)
global TN, conflicts, preference
GTN (number)
IR ICR VMR  ICR conversion, VMR conversion (translation)
inline expansion, call
kill (to make dead)
known function
LAMBDA
leaf
let call
lifetime analysis, live (tn, variable)
load tn
LOCS (passing, return locations)
local call
local TN, conflicts, (or just used in one block)
location (selection)
LTN (number)
main entry
mess-up (for cleanup)
more arg (entry)
MV
non-local exit
non-packed SC, TN
non-set variable
operand (to vop)
optimizer (in icr optimize)
optional-dispatch
pack, packing, packed
pass (in a transform)
passing
	locations (value)
	conventions (known, unknown)
policy (safe, fast, small, ...)
predecessor block
primitive-type
reaching definition
REF
representation
	selection
	for value
result continuation (for function)
result type assertion (for template) (or is it restriction)
restrict
	a TN to finite SBs
	a template operand to a primitive type (boxed...)
	a tn-ref to particular SCs

return (node, vops)
safe, safety
saving (of registers, costs)

  ** SB  A Storage Base: a physical storage resource such as a register set
         or stack frame.

  ** SC  A Storage Class: the storage base that storage is allocated in and
         information used to select locations within the SB.

SC (restriction)
semi-inline
side-effect
	in ICR
	in VMR
sparse set
splitting (of VMR blocks)
SSET
SUBPRIMITIVE
successor block
tail recursion
	tail recursive
	tail recursive loop
	user tail recursion

  ** template:  a particular IR2 coding strategy for a known function

  ** TN:  Temporary Name

TNBIND
TN-REF
transform (source, ICR)
type
	assertion
	inference
		top-down, bottom-up
	assertion propagation
        derived, asserted
	descriptor, specifier, intersection, union, member type
        check
type-check (in continuation)
UNBOXED (boxed) descriptor
unknown values continuation
unset variable
unwind-block, unwinding
used value (dest)
value passing
VAR
VM
VOP
XEP

FIX general
symlink

\end{description}
]#
