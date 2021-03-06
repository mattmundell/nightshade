;;; Implementation-independent facilities used for defining the compiler's
;;; interface to the VM in a given implementation.

(in-package :c)

(export '(template-or-lose sc-or-lose sb-or-lose sc-number-or-lose
	  meta-sc-or-lose meta-sb-or-lose meta-sc-number-or-lose
	  primitive-type-or-lose note-this-location note-next-instruction))

;;; Template-Or-Lose  --  Internal
;;;
;;; Return the template having the specified name, or die trying.
;;;
(defun template-or-lose (x &optional (backend *target-backend*))
  (the template
       (or (gethash x (backend-template-names backend))
	   (error "~S is not a defined template." x))))

;;; SC-Or-Lose, SB-Or-Lose, SC-Number-Or-Lose  --  Internal
;;;
;;; Return the SC structure, SB structure or SC number corresponding to a
;;; name, or die trying.
;;;
(defun sc-or-lose (x &optional (backend *target-backend*))
  (the sc
       (or (gethash x (backend-sc-names backend))
	   (error "~S is not a defined storage class." x))))
;;;
(defun sb-or-lose (x &optional (backend *target-backend*))
  (the sb
       (or (gethash x (backend-sb-names backend))
	   (error "~S is not a defined storage base." x))))
;;;
(defun sc-number-or-lose (x &optional (backend *target-backend*))
  (the sc-number (sc-number (sc-or-lose x backend))))

;;; META-SC-OR-LOSE, META-SB-OR-LOSE, META-SC-NUMBER-OR-LOSE  --  Internal
;;;
;;; Like the non-meta versions, but go for the meta-compile-time info.
;;; These should not be used after load time, since compiling the compiler
;;; changes the definitions.
;;;
(defun meta-sc-or-lose (x)
  (the sc
       (or (gethash x (backend-meta-sc-names *target-backend*))
	   (error "~S is not a defined storage class." x))))
;;;
(defun meta-sb-or-lose (x)
  (the sb
       (or (gethash x (backend-meta-sb-names *target-backend*))
	   (error "~S is not a defined storage base." x))))
;;;
(defun meta-sc-number-or-lose (x)
  (the sc-number (sc-number (meta-sc-or-lose x))))


;;;; Side-Effect Classes.

(def-boolean-attribute vop
  any)


;;;; Move/coerce definition.

;;; COMPUTE-MOVE-COSTS  --  Internal
;;;
;;; Compute at compiler load time the costs for moving between all SCs that
;;; can be loaded from FROM-SC and to TO-SC given a base move cost Cost.
;;;
(defun compute-move-costs (from-sc to-sc cost)
  (declare (type sc from-sc to-sc) (type index cost))
  (let ((to-scn (sc-number to-sc))
	(from-costs (sc-load-costs from-sc)))
    (dolist (dest-sc (cons to-sc (sc-alternate-scs to-sc)))
      (let ((vec (sc-move-costs dest-sc))
	    (dest-costs (sc-load-costs dest-sc)))
	(setf (svref vec (sc-number from-sc)) cost)
	(dolist (sc (append (sc-alternate-scs from-sc)
			    (sc-constant-scs from-sc)))
	  (let* ((scn (sc-number sc))
		 (total (+ (svref from-costs scn)
			   (svref dest-costs to-scn)
			   cost))
		 (old (svref vec scn)))
	    (unless (and old (< old total))
	      (setf (svref vec scn) total))))))))


;;;; Primitive type definition.

;;; PRIMITIVE-TYPE-OR-LOSE  --  Interface
;;;
;;; Return the primitive type corresponding to the specified name, or die
;;; trying.
;;;
(defun primitive-type-or-lose (name &optional (backend *target-backend*))
  (the primitive-type
       (or (gethash name (backend-primitive-type-names backend))
	   (error "~S is not a defined primitive type." name))))

;;; SC-ALLOWED-BY-PRIMITIVE-TYPE  --  Interface
;;;
;;; Return true if SC is either one of Ptype's SC's, or one of those SC's
;;; alternate or constant SCs.
;;;
(defun sc-allowed-by-primitive-type (sc ptype)
  (declare (type sc sc) (type primitive-type ptype))
  (let ((scn (sc-number sc)))
    (dolist (allowed (primitive-type-scs ptype) nil)
      (when (eql allowed scn)
	(return t))
      (let ((allowed-sc (svref (backend-sc-numbers *backend*) allowed)))
	(when (or (member sc (sc-alternate-scs allowed-sc))
		  (member sc (sc-constant-scs allowed-sc)))
	  (return t))))))


;;;; Emit function generation.

(defconstant max-vop-tn-refs 256)

(defvar *vop-tn-refs* (make-array max-vop-tn-refs :initial-element nil))
(defvar *using-vop-tn-refs* nil)

(defun flush-vop-tn-refs ()
  (unless *using-vop-tn-refs*
    (fill *vop-tn-refs* nil)))

(pushnew 'flush-vop-tn-refs *before-gc-hooks*)

(defconstant sc-bits (integer-length (1- sc-number-limit)))

(defun emit-generic-vop (node block template args results &optional info)
  (%emit-generic-vop node block template args results info))

(defun %emit-generic-vop (node block template args results info)
  (let* ((vop (make-vop block node template args results))
	 (num-args (vop-info-num-args template))
	 (last-arg (1- num-args))
	 (num-results (vop-info-num-results template))
	 (num-operands (+ num-args num-results))
	 (last-result (1- num-operands))
	 (ref-ordering (vop-info-ref-ordering template)))
    (declare (type vop vop)
	     (type (integer 0 #.max-vop-tn-refs)
		   num-args num-results num-operands)
	     (type (integer -1 #.(1- max-vop-tn-refs)) last-arg last-result)
	     (type (simple-array (mod #.max-vop-tn-refs) (*)) ref-ordering))
    (setf (vop-codegen-info vop) info)
    (let ((refs *vop-tn-refs*)
	  (*using-vop-tn-refs* t))
      (declare (type (simple-vector #.max-vop-tn-refs) refs))
      (do ((index 0 (1+ index))
	   (ref args (and ref (tn-ref-across ref))))
	  ((= index num-args))
	(setf (svref refs index) ref))
      (do ((index num-args (1+ index))
	   (ref results (and ref (tn-ref-across ref))))
	  ((= index num-operands))
	(setf (svref refs index) ref))
      (let ((temps (vop-info-temps template)))
	(when temps
	  (let ((index num-operands)
		(prev nil))
	    (dotimes (i (length temps))
	      (let* ((temp (aref temps i))
		     (tn (if (logbitp 0 temp)
			     (make-wired-tn nil
					    (ldb (byte sc-bits 1) temp)
					    (ash temp (- (1+ sc-bits))))
			     (make-restricted-tn nil (ash temp -1))))
		     (write-ref (reference-tn tn t)))
		(setf (aref refs index) (reference-tn tn nil))
		(setf (aref refs (1+ index)) write-ref)
		(if prev
		    (setf (tn-ref-across prev) write-ref)
		    (setf (vop-temps vop) write-ref))
		(setf prev write-ref)
		(incf index 2))))))
      (let ((prev nil))
	(flet ((add-ref (ref)
		 (setf (tn-ref-vop ref) vop)
		 (setf (tn-ref-next-ref ref) prev)
		 (setf prev ref)))
	  (declare (inline add-ref))
	  (dotimes (i (length ref-ordering))
	    (let* ((index (aref ref-ordering i))
		   (ref (aref refs index)))
	      (if (or (= index last-arg) (= index last-result))
		  (do ((ref ref (tn-ref-across ref)))
		      ((null ref))
		    (add-ref ref))
		  (add-ref ref)))))
	(setf (vop-refs vop) prev))
      (let ((targets (vop-info-targets template)))
	(when targets
	  (dotimes (i (length targets))
	    (let ((target (aref targets i)))
	      (target-if-desirable (aref refs (ldb (byte 8 8) target))
				   (aref refs (ldb (byte 8 0) target))))))))
    (values vop vop)))


;;;; Function translation stuff.

;;; Adjoin-Template  --  Internal
;;;
;;; Add Template into List, removing any old template with the same name.
;;; We also maintain the increasing cost ordering.
;;;
(defun adjoin-template (template list)
  (declare (type template template) (list list))
  (sort (cons template
	      (remove (template-name template) list
		      :key #'template-name))
	#'<=
	:key #'template-cost))


;;; Template-Type-Specifier  --  Internal
;;;
;;; Return a function type specifier describing Template's type computed
;;; from the operand type restrictions.
;;;
(defun template-type-specifier (template)
  (declare (type template template))
  (flet ((convert (types more-types)
	   (flet ((frob (x)
		    (if (eq x '*)
			't
			(ecase (first x)
			  (:or `(or ,@(mapcar #'(lambda (type)
						  (type-specifier
						   (primitive-type-type
						    type)))
					      (rest x))))
			  (:constant `(constant-argument ,(third x)))))))
	     `(,@(mapcar #'frob types)
	       ,@(when more-types
		   `(&rest ,(frob more-types)))))))
    (let* ((args (convert (template-arg-types template)
			  (template-more-args-type template)))
	   (result-restr (template-result-types template))
	   (results (if (eq result-restr :conditional)
			'(boolean)
			(convert result-restr
				 (cond ((template-more-results-type template))
				       ((/= (length result-restr) 1) '*)
				       (t nil))))))
      `(function ,args
		 ,(if (= (length results) 1)
		      (first results)
		      `(values ,@results))))))


;;;; Random utilities.

;;; NOTE-THIS-LOCATION  --  Interface
;;;
(defun note-this-location (vop kind)
  "NOTE-THIS-LOCATION VOP Kind

   Note that the current code location is an interesting (to the debugger)
   location of the specified Kind.  VOP is the VOP responsible for this
   code.  This VOP must specify some non-null :SAVE-P value (perhaps
   :COMPUTE-ONLY) so that the live set is computed."
  (let ((lab (gen-label)))
    (emit-label lab)
    (note-debug-location vop lab kind)))

;;; NOTE-NEXT-INSTRUCTION -- Interface
;;;
(defun note-next-instruction (vop kind)
  "NOTE-NEXT-INSTRUCTION VOP Kind
   Similar to NOTE-THIS-LOCATION, except the use the location of the next
   instruction for the code location, wherever the scheduler decided to put
   it."
  (let ((loc (note-debug-location vop nil kind)))
    (new-assem:emit-postit #'(lambda (segment posn)
			       (declare (ignore segment))
			       (setf (location-info-label loc) posn))))
  (undefined-value))


#[ Virtual Machine

[ VM Introduction               ]
[ Data types, storage resources ]
[ Characters                    ]
[ Symbols                       ]
[ Lists                         ]
[ Numbers (VM)                  ]
[ Arrays (VM)                   ]
[ Structures                    ]
[ Runtime environment           ]
[ Functions                     ]
[ Non-local exits               ]
]#

#[ VM Introduction

(defun gvp (f)
  (with-open-file (s f :direction :output :if-exists :supersede)
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (format s "~A~%" k))
	     (c::backend-template-names c::*backend*))))

== Scope and Purpose ==

This document describes the Virtual Machine that serves as the basis for the
portable implementation of \ccl.  The Virtual Machine (hereafter referred to as
the VM) provides a layer of abstraction that hides low-level details of
hardware and implementation strategy, while still revealing enough of the
implementation so that most of the system can be written at the VM level or
above.

\begin{comment}

{XXX Shouldn't specify VOPs.  Instead, should specify which \clisp functions
are primitive and which subprimitives exist.  Isn't really anyone's business
which VOPs actually exist.  Each primitive function or subprimitive is
implemented either as a VOP or as expansion into Lisp code, at the particular
implementation's discretion.

From this point of view, the document is expressing the contract that the Lisp
level code outside of the compiler must satisfy.  All functions must ultimately
be defined in terms of primitive functions and sub-primitives.

The responsibility of the compiler is to implement these primitive operations,
and also to implement special forms, variables and function calling.

VOPs emitted by the hard-wired translators for non-function nodes are a
somewhat different story.  Each implementation will presumably implement all
these VOPs in order to avoid having to rewrite IR2 translation.  We also need
to spend quite a bit of time discussing the semantics of these operations,
since they don't just correspond to some \clisp function with type constraints.

Hard-wired stuff:

function call
variable access:
  global
  function
  constant
  closure
  local
closure creation
non-local exit
special binding/unbinding
TN hacking:
  move VOPs
  TN address (???)
Conditionals:
  Basic conditionals: EQ, ...
  Interface to generation of other conditional VOPs.

Some VOPs don't need to be implemented at all:
  VOPs to delimit the lifetimes of big stack TNs such as catch blocks
  Others?  Move VOPs might be defined in terms of an implementation supplied
  move routine, since we probably also need this info outside of VOP generators
  so that implicit moves can be generated.


Type testing/checking (somehow)

}

What this document talks about:

Interface between compiler front-end and back end. (VOPs)
   Primitive \clisp operations directly supported by the VM.
   Support for complex language features such as function call.

Sub-primitives that allow system code to do things not possible in \clisp.

Descriptions of how the current \ccl system uses VM facilities, especially
non-standard ones accessed through sub-primitives.

Notes about known portability problems.

Guidelines for writing portable \ccl system code.  To some degree these
guidelines are implied by statements that certain things are true of \ccl
system code.

Descriptions of data structures that are not directly used by the VM, such as
debug information and Core files.

Descriptions of data structures that are directly used by the VM, such as
symbols and arrays.


Who should read it:

People who want to port \ccl.
People who want to understand the compiler.
People who want to understand how \ccl works.
People who need to write portable \ccl system code.
People such as debugger writers who need to access \ccl\t()'s internal data
structures.

What it won't do:

Tell you things that are obviously implementation dependent, such as type
systems or memory management disciplines.  See the the various implementation
VM documents.

Tell you only what you need to know.  Programmers shouldn't exploit properties
of the VM documented here unless there is no way to do the same thing in
portable \clisp.

Tell you how the compiler works.  In order to understand some of the subtleties
of VOP descriptions, you will have to understand the IR2 representation and how
it fits into the rest of the compiler.

Tell you anything about \clisp semantics.  When some part of the VM has a
direct relationship to \clisp semantics, the relationship will be directly
stated using \clisp terminology, since a restatement of the semantics is likely
to be inaccurate or misleading.  Exceptions will be made only when some
implication of the \clisp semantics is non-obvious.

Tell you everything about how \ccl works.  This document only offers
information that is likely to be needed by programmers doing a port or writing
system code; portable, self-contained parts of the system are totally ignored.
This document deliberately avoids replicating information that is easily
available in the system sources, since such replicated information is always
incorrect somewhere.  In some cases, a forwarding pointer to the appropriate
source will be given.


Things the VM won't do:

The VM specification does not totally solve the problem of porting \ccl, since
it is inevitable that it will not map cleanly to all possible combinations of
hardware and operating systems.  The VM should not be regarded as being cast in
concrete, since changes in many characteristics would only affect a tiny
fraction of the system sources.

One current major problem with porting is that large pieces of functionality
are entirely within the VM, and would need to be reimplemented for each port.
A major goal for future work on the system is moving code out of the VM, both
by supporting a "fast-call" convention that allows reasonable use of Lisp in
the out of line implementation of VOPs, and by having a "bugout" mechanism that
allows the VM to call Lisp functions to implement the hard cases in some VOPs.

The VM is designed to support conventional, untagged, general register
architectures.  Suitably lobotomized, it could be mapped to less flexible
hardware such as "Lisp machines", but the compiler would have serious
difficulties supporting stack architectures.

The VM does not support concurrent lightweight processes.  Locking primitives
and deep-binding of specials would be needed.

The VM does not deal with operating systems interface issues at all.  A minimal
port would require implementing at least file and terminal I/O streams.  \ccl
implements system interfaces using Aliens and other facilities built on top of
them.

\end{comment}


Major components:
 * Specific virtual operations implemented by the VM (VOPs).  VOPs are
   primarily the concern of the compiler, since it translates Lisp code into
   VOPs and then translates VOPs into the implementation.

 * Sub-primitives that are used by Lisp code needing to perform operations
   below the Lisp level.  The compiler implements some sub-primitives directly
   using VOPs, while others are translated into Lisp code.  Sub-primitives provide
   a layer of insulation between the Lisp system code and the VM, since the Lisp
   code may assume the existence of operations that are not implemented directly
   by the VM.  Only sub-primitives with fairly portable semantics are documented
   here.  Others are in implementation-specific VM documentation.


    Not all sub-primitives are VOPs, and most VOPs are not sub-primitives.


=== VOP base name rules ===

The names of VOPs that implement functions are based on the function name.
Other VOPs may use any base that doesn't conflict with a function name.  There
are some rules used to obtain the base name for related operations.

To get the name of a setting operation, replace the string "{\tt ref}" in the name
with "{\tt set}".  If "{\tt ref}" doesn't appear in the name, add the prefix "{\tt set-}" to the
base name.  For example, {\tt svref} becomes {\tt svset}, and {\tt symbol-value}
becomes {\tt set-symbol-value}.

To get the name of a conditional VOP from the name of a predicate, add the
prefix "{\tt if-}" to the predicate name.  For example, {\tt eq} becomes {\tt if-eq}.
{\tt eq} by itself would be a VOP that returned true or false value.

Some operations check for some error condition, magically signalling the error
through an implicit control transfer.  These operations are prefixed with
"{\tt check-}", as in {\tt check-fixnum} and {\tt check-bound}.



=== VOP name prefixes and suffixes ===

Prefixes and suffixes are added to the base to get the names of variant
versions of the VOP.  The fully general VOP name looks like this:
\begin{format}
   {"{\tt small-}" | "{\tt fast-}"} {\it name}{"{\tt -c}" {\it info}}{"{\tt /}" {\it type}{"{\tt =>}" {\it result-type}}
\end{format}
The "{\tt small-}" and "{\tt fast-}" prefixes indicates that the VOP does minimal
safety checking and is optimized for space or speed, respectively.  The absence
of a prefix indicates the safest (or only) version.  Usually if the "{\tt small-}"
VOP exists, it will be a synonym for either the fast version or the safe
version, depending on which is smaller.

The "{\tt -c}" suffix indicates that the some info that is passed as a normal
argument to the base version of the VOP is passed as Codegen-Info in this
version.  A typical use would be for VOPs where it is important to use a
different version when one of the arguments is a compile time constant.
{\it info} is some (possibly null) string that indicates which "{\tt -c}" variant
is involved.

The "{\tt /}{\it type}" suffix asserts that all operands that could be of {\it type} are.
For example, {\tt +/fixnum} adds two fixnums returning a fixnum, while
{\tt length/simple-vector} finds the length of a simple vector, but the result isn't
a simple vector.

The "{\tt =>}{\it result-type}" suffix supplies a result type assertion on the
operation.

A not totally silly example of all these modifiers simultaneously is
 {\tt fast-+-c/fixnum=>integer}.  This operation would this operation adds two
fixnums, one of which is a constant passed as codegen info, resulting in an
integer.  The implementation is optimized for speed at the expense of space and
safety.
]#


#[ Data Types, Storage Resources


== Lisp Objects ==
\index{Lisp objects}

A Lisp object is fixed-size data structure that is organized in a way mandated
by the VM implementation.  The fixed format allows the VM to determine the type
of the object.  \comment<Virtual type?  VM type?  Implementation type?
...provides the VM enough information about the type of the object for the VM
to implement the VM-level semantics...  ...supports the "dynamic types"...>

Lisp objects are stored in locations known as cells.


Has major types: immediate and non-immediate.
Non-immediate objects may have a subtype.
Non-immediate types:
  symbol (nil may be weird)
  cons
  ratio
  complex
  some float types
  g-vector
  i-vector
  string
  bit-vector
  environment (always has subtype)
  array header
  bignum
  structure
  pc (code vector)
  stack closure (control stack pointer)

Non-immediate objects are allocated in "type spaces".  The type space of an
object is characterized by a small integer known as the type code.  Any two
objects of one of the above boxed types will always have the same type code.
{But not really...  Some types might be allocated in different type spaces at
different times. (?)}

The type code doesn't totally describe the object.  In general, subtype
information may be involved.


Immediate types:
  character
  fixnum
  unbound trap
  short float



== Type VOPs ==

We consider control transfer to be the fundamental result of comparison, rather
than anything such as a condition code.  Although most compilers with whizzy
register allocation seem to explicitly allocate and manipulate the condition
codes, it seems that any benefit is small in our case.  This is partly because
our VOPs are at a somewhat higher level, making it difficult to tell which VOPs
do and don't trash the the CC.  Explicitly incorporating condition codes in our
VM also introduces another architecture dependency.

At the IR2 level, we have a class of IF-XXX VOPs which transfer control to one
of two places on the basis of some test on the operands.  When generating code
for a predicate, we peek at the destination IF node to find where to transfer
control to.

The exact representation of type tests in IR2 will be fairly implementation
dependent, since it will depend on the specific type system for the given
implementation.  For example, if an implementation can test some types with a
simple tag check, but other types require reading a field from the object in
addition, then the two different kinds of checks should be distinct at the VOP
level, since this will allow the VOP cost and storage information to be more
accurate.  Generation of type tests should be factored out of code which would
otherwise be more portable.  Probably the IR2 translator for TYPEP and the type
check generation code are the only places that should know about how type tests
are represented in IR2.

if-type (object)
if-type-range
    If-Type Tests whether Object has the type code that is passed in the
    codegen info.  If-Type-Range tests for a range of type codes.

{small, fast} if-vector-type (object)
    Test that Object is either of the specified type code, or is a 1d array
    header with data having the specified type code.

if-vector-subtype (object)
    Test the subtype field of a vector-like object.  It is assumed that the
    object has already been determined to be vector-like.

if-fixnump (object)
if-short-float-p
if-characterp
    The rationale behind having these as separate VOPs is that they are likely
    to be immediate types, and thus may have bizzare type schemes.

if-consp (object)
if-listp
    We have distinct operations for these predicates since one or the other
    isn't a simple tag test, but we don't know which one.

if-rationalp (object)
if-floatp
if-integerp
if-numberp
if-vectorp
if-functionp
    The rationale behind having these operations is that they may take a lot of
    code, so it is reasonable to put them out of line.



== Type Sub-primitives ==

change-type (object) => result
    Change the type of an object according to codegen info.  The meaning of
    this is highly type-system dependent, but it doesn't matter, since the
    compiler will never emit this VOP directly.  The only way that it can show
    up is through %Primitive.
get-type


Storage resources:

Boxed and unboxed locations:
Non-immediate objects may not be stored in unboxed locations.
Things not lisp objects may not be stored in boxed locations.

Control stack is boxed.
Optional number stack is unboxed.
Heap environment is boxed.
Fixed number of registers, some boxed and some unboxed.

PCs may be stored on the control stack or in boxed registers, subject to the
constraint that a corresponding environment is also stored.  Locations
containing PCs don't need to be zeroed when they are no longer used; nothing
bad will happen if an old PC is unaccompanied by an environment.


 + Trap: Illegal object trap.  This value is used in symbols to signify an
   undefined value or definition.
]#


#[ Characters

Character is an immediate type.  Characters are manipulated primarily by
converting into an integer and accessing these fields:

  # %character-code-byte: The character code.  This is effectively required to
    start at bit 0, since \cl equates {\tt char-int} to {\tt char-code} when there is
    no bits or font.  All current \ccl systems use ASCII for the character codes,
    and define {\tt \#\newline} to be a linefeed, but system code should not count on
    this.

  # %character-control-byte: The character bits.  Character bits are used by
    the editor to describe modifiers in keyboard events, but there is no
    assumption of any general portable significance of character bits.

  # %character-font-byte: The character font.  This is not used by \ccl, and is
    not particularly useful.


Characters should be converted to and from integers by using the \clisp
{\tt char-int} and {\tt int-char} functions, which the compiler translates into
these VOPs:
\begin{example}
char-int (char) => int
int-char (int) => char
\end{example}
In the common case where Char is known to be a {\tt string-char}, these
operations are equivalent to {\tt char-code} and {\tt code-char}.  In addition to
providing a portable interface to character conversion, the VOP representation
of this type conversion allows the compiler to avoid unnecessary boxing and
unboxing of character objects.

Existing code explicitly converts fixnums to characters by using the
Make-Immediate-Type sub-primitive with %Character-Type.  Currently conversion
of characters to fixnums is rather confused.  Originally, characters were a
subtype of the Misc type code, and the result of the Make-Fixnum sub-primitive
had to be masked with {\tt %character-int-mask}; some code still does this, while
other code may not.

Character comparisons could be implemented by doing numeric comparisons on the
result of {\tt char-int}, or by using {\tt eq} in the case of {\tt char=}, but this
can result in unnecessary type conversions.  Instead, the compiler uses these
conditional VOPs:
    if-char= (x y)
    if-char< (x y)
    if-char> (x y)
]#


#[ Symbols

Symbols are currently fairly boring, containing only the obvious slots:

  % %symbol-value-slot

    The current dynamic value of this symbol.  If the symbol is currently
    unbound, then the value of this slot is the unbound marker.

  % %symbol-function-slot

    The global function function definition of this symbol.  If the symbol
    is not fbound, then this slot holds the unbound marker.

  % %symbol-plist-slot, %symbol-name-slot, %symbol-package-slot

    The property list, print name and package for this symbol.

== Sub-primitives ==

The {\tt alloc-symbol} sub-primitive allocates a new symbol object.  {\it name} is
the simple-string that is to be the name of the symbol.
    alloc-symbol (name) => symbol

The {\tt set-symbol-package} sub-primitive is used by system code that must set
the symbol package.

    set-symbol-package (symbol new-value)

== Accessor VOPs ==

These VOPs read the global symbol value and definition cells.  {\tt constant-ref}
may only be used on symbols that have been defined to be constants.  Since a
constant cannot change in value and cannot be dynamically bound, the compiler
may be able to compile uses of {\tt constant-ref} more efficiently.  Unsafe
versions of these VOPs may not check for the slot being unbound, which the
corresponding \clisp functions are required to do.

    {small, fast} symbol-value (symbol) => value
    {small, fast} constant-ref (symbol) => value
    {small, fast} symbol-function (symbol) => value

These VOPs set the global symbol value and definition cells.  {\tt makunbound}
and {\tt fmakunbound} are implemented by setting the value to the unbound marker.

    {small, fast} set-symbol-value (symbol new-value)
    {small, fast} set-symbol-function (symbol new-value)

The Lisp accessors for other symbol slots are translated into uses of the
{\tt slot-ref} and {\tt slot-set} VOPs.

== Special Binding ==

These VOPs implement dynamic binding of special variables using shallow
binding.  {\tt bind} binds {\it symbol} to the specified {\it value}, while
{\tt unbind} undoes the most recent {\it count} special bindings on the binding
stack.

    bind (symbol value)
    unbind (count)

== Property Lists ==

The {\tt get} VOP implements the corresponding \clisp function, while {\tt put}
implements its setf-inverse.

    get (symbol indicator default) => value
    put (symbol indicator value)
]#


#[ Lists

cons

list<n> (elt0 ... elt<n-1>) => list
list (elt0 ... elt<n-1> more-elts) => list
    For some small N, we have fixed-arg versions of List.  For larger lists, we
    pass in additional elements in a stack TN (possibly required to be on stack
    top).  List* is similar.


These VOPs implement the corresponding \clisp functions:
    {small, fast} car (list) => value
    {small, fast} cdr (list) => value

These VOPs set the car or cdr of a cons:

    {small, fast} set-car (cons new-value)
    {small, fast} set-cdr (cons new-value)

These VOPs implement the \clisp {\tt assoc} and {\tt member} functions with test
functions of {\tt eql} and {\tt eq}:

    assoc (item alist) => cons-or-nil
    assq (item alist) => cons-or-nil
    member (item list) => cons-or-nil
    memq (item list) => cons-or-nil

{\tt getf} implements the corresponding \clisp function, while {\tt putf} is used
to implement its setf-inverse.  {\tt putf} returns the new value for the list so
that it may stored back into the place.
    getf (list indicator default) => value
    putf (list indicator new-value) => list
]#


#[ Numbers (VM)

\index{Fixnum format}
  # Fixnum

    An N-bit two's complement integer.

\index{Short float format}
  # Short-Float

    An immediate float format.

\index{Bignum format}
\label{Bignums}
  # Bignum

    Bignums are infinite-precision integers, represented somehow.

\index{Flonum format}
\index{Floating point formats}
  # Floats

    Floats are stored as consecutive words of bits.

\index{Ratio format}
  # Ratio

    Ratios are stored as two consecutive words of Lisp objects, which
    should both be integers.

\index{Complex number format}
  # Complex

    Complex numbers are stored as two consecutive words of Lisp objects,
    which should both be numbers.

== Number VOPs ==

integer-length
{small, fast} integer-length/fixnum

float=>xxx-float

realpart
lmagpart
numerator
denominator
decode-float
{small, fast} decode-float/xxx-float
scale-float
{small, fast} scale-float/xxx-float

if-= (x y)
{small, fast} if-=/fixnum
{small, fast} if-=/xxx-float
    Do numeric comparison of X and Y.  The codegen-info contains the
    continuations to transfer to in the true and false cases.  Same for <, >.

+ (x y) => z
{small, fast} +/fixnum
{small, fast} +/fixnum=>integer
{small, fast} +/xxx-float
    Same for -, *.   Fixnum multiplication by a constant power of 2 (or near
    power of 2) can be done by a transform.

/ (x y) => z
{small, fast} //xxx-float

negate
{small, fast} negate/fixnum
{small, fast} negate/fixnum=>integer
{small, fast} negate/xxx-float
    Ditto for Abs.

truncate (x y) => q r
{small, fast} truncate/fixnum

logand (x y) => z
{small, fast} logand/fixnum
    Ditto for logior, logxor.

lognot (n) => z
{small, fast} lognot/fixnum

ash (n x) => z
{small, fast} ash/fixnum
{small, fast} ash-c/fixnum

ldb
dpb
mask-field
deposit-field
    These will only be used as a last resort.  There should be transforms that
    turn fixnum operations with constant byte-specifiers into standard logical
    operations.

== Number Sub-primitives ==


alloc-bignum
make-complex
make-ratio
lsh
logldb
logdpb
]#


#[ Arrays (VM)

\cl arrays can be represented in a few different ways in \rtccl --
different representations have different performance advantages.  Simple
general vectors, simple vectors of integers, and simple strings are basic \rtccl
 data types, and access to these structures is quicker than access to
non-simple (or ``complex'') arrays.  However, all multi-dimensional arrays in
\rtccl are complex arrays, so references to these are always through a
header structure.


Once a vector has been allocated, it is possible to reduce its length by using
the Shrink-Vector sub-primitive, but never to increase its length, even back to
the original size, since the space freed by the reduction may have been
reclaimed.


=== Arrays ===
\label{Arrays}
\index{Arrays}

An array header is identical in form to a G-Vector.  At present, the following
subtype codes are defined:
 # Normal.
 # Array is displaced to another array (which may be simple).
   The entries in the header-vector are interpreted as follows:

\index{Array header format}
 + 0 Data Vector: This is a pointer to the I-Vector, G-Vector, or string that
   contains the actual data of the array. In a multi-dimensional array, the
   supplied indices are converted into a single 1-D index which is used to access
   the data vector in the usual way.  If the array is displaced, then this is
   the array displaced to, which may be an array header.  In general, array
   access must loop until it finds an actual data vector.

 + 1 Number of Elements: This is a fixnum indicating the number of elements for
   which there is space in the data vector.

 + 2 Fill Pointer: This is a fixnum indicating how many elements of the data
   vector are actually considered to be in use.  Normally this is initialized to
   the same value as the Number of Elements field, but in some array applications
   it will be given a smaller value.  Any access beyond the fill pointer is
   illegal.

 + 3 Displacement: This fixnum value is added to the final code-vector index
   after the index arithmetic is done but before the access occurs.  Used for
   mapping a portion of one array into another.  For most arrays, this is 0.

 + 4 Range of First Index: This is the number of index values along the first
   dimension, or one greater than the largest legal value of this index (since the
   arrays are always zero-based).	A fixnum in the range 0 to 2\+{24}-1.  If any
   of the indices has a range of 0, the array is legal but will contain no data
   and accesses to it will always be out of range.  In a 0-dimension array, this
   entry will not be present.

 + 5 - N:  Ranges of Subsequent Dimensions

The number of dimensions of an array can be determined by looking at the length
of the array header.  The rank will be this number minus 6.  The maximum array
rank is 65535 - 6, or 65529.

The ranges of all indices are checked on every access, during the conversion to
a single data-vector index.  In this conversion, each index is added to the
accumulating total, then the total is multiplied by the range of the following
dimension, the next index is added in, and so on.  In other words, if the data
vector is scanned linearly, the last array index is the one that varies most
rapidly, then the index before it, and so on.

== Array VOPs ==

alloc-bit-vector
alloc-i-vector
alloc-string
alloc-g-vector
    Initialized and uninitialized versions?


length (sequence) => size
{small, fast} length/vector
{small, fast} length/simple-vector
{small, fast} length/simple-string
{small, fast} length/simple-bit-vector

aref1 (vector index) => value
{small, fast} aref1/simple-vector
{small, fast} aref1/simple-string
{small, fast} aref1/simple-bit-vector
{small, fast} aref1/simple-array-XXX-float

aset1 (vector index new-value)
{small, fast} aset1/simple-vector
{small, fast} aset1/simple-string
{small, fast} aset1/simple-bit-vector
{small, fast} aset1/simple-array-XXX-float

{small, fast} aref1/simple-array-unsigned-byte (vector index) => value
{small, fast} aset1/simple-array-unsigned-byte (vector index new-value)
    Byte size is codegen info.

aref<N> (array index0 ... index<n-1>) => value
aset<N> (array index0 ... index<n-1> new-value)
    For some small value of N.  Of course, higher dimensional arrays can also
    be specialized in seven different ways....  Multi-dimensional simple array
    reference with known dimensions can be open-coded using a transform (useful
    for benchmarks.)


== Array Sub-primitives ==

alloc-array
vector-subtype
set-vector-subtype
vector-access-code
set-vector-access-code
shrink-vector

typed-vref
typed-vset

header-length (header) => size
header-ref (header index) => value
header-set (header index new-value)

bit-bash
byte-blt
{reverse-}find-character
{reverse-}find-character-with-attribute
{reverse-}string-compare
sxhash-simple-string
sxhash-simple-substring
]#


#[ Structures

{small, fast} structure-ref (s) => value
{small, fast} structure-set (s new-value)
    Read and write structure slots.  Defstruct slot description is in codegen
    info.

alloc-structure
]#


#[ Runtime Environment
\label{Runtime}


== Register Allocation ==
\index{Register allocation}

The main idea is to globally allocate only those registers with global
significance.

We permanently dedicate the CONT register to point to the current control stack
environment.  This is the "frame pointer" in standard terminology.  It isn't
possible to get pack to allocate this register on an as-needed basis due to the
classic phase-ordering problem.  We need to know if TNs are allocated on the
stack before we can determine tell how badly we need a frame pointer register.
This is of little significance with the control stack environment, since we
almost always need one, and if there are any stack TNs, we must allocate the
frame pointer in a register, since there is nowhere else to put it.  The
problem is more severe with a number stack environment pointer.  We can't
dedicate a register to it, since we usually don't have any TNs on the number
stack.  The only easy solution is to always allocate the number stack
environment pointer on the control stack.  This really isn't too bad, when you
compare the cost of doing an extra memory reference to get at the number stack
to the cost of number-consing.

We also dedicate the ENV register to the current constant pool.  It would be
possible to explicitly allocate the constant pointer as needed if we explicitly
represented non-immediate constant access by a VOP, but this would be extra
work, and there are major advantages to representing all constants using TNs.
Another potential efficiency advantage is since the same constant pool is
shared by all the code in a component, we need only initialize ENV on entry to
the component.  When we make local calls, we don't have to do anything to make
the constants available to the callee.

Since the constant pool will also contain the code vector and the debug
info, having it in a known place may make life easier for GC and the
debugger.  We may not be able to count on it too much, though, since ENV
holds other things while calls are in progress, and might be pretty random
if we jumped into hyperspace.


Runtime environment:

CONT: the current control stack context.
PC is assumed to be accessible to the debugger when an error happens.
Current-Catch: pointer to the current catch frame.  Format of frame is assumed.
Current-Unwind-Protect: current unwind protect frame.  Similar to catch.

If shallow-bind, binding stack and binding stack pointer.
If deep-bind, current special binding.  Format of binding frame assumed.

Everything depends on the current environment, which is CONT.


PC
OLD-CONT
ENV
A<n>
CONT
CS


== Other Dynamic State ==

There are some dynamic state variables that are stored in known memory
locations, rather than having a dedicated register:
 + binding stack pointer: The current pointer to the top of the binding stack.

 + current catch: The pointer to the current catch block.

 + current unwind-protect: The pointer to the current unwind-protect block.


== Control-Stack Format ==
\label{Control-Stack-Format}
\index{Control-stack format}

The control stack contains only Lisp objects.  Every object pointed to by an
entry on this stack is kept alive.

The \rtccl control stack does not have a rigid frame structure.  The compiler
is allowed a large amount of freedom in the use of the stack so that it choose
the best calling sequences.  Mostly the compiler is the only system that cares
how the stack is laid out, so this isn't a big problem.  See chapter
\ref{debug-info} for a description of the structures which allow the debugger
to parse the stack.


== Values Passing Conventions ==

The first {\it nregs} arguments are passed in registers, where nregs is an
implementation dependent constant.  Any additional arguments are the block of
storage between CONT and CS on the control stack.  The first nregs locations in
this block of storage are unused so that register more-args can be stored on
the stack without having to BLT the stack values up.

Returning unknown values are passed in a similar way, but the stack values
block is between OLD-CONT and CS.  There isn't any underneath the values: on
return OLD-CONT is always what CS was when the function was called.  The
function returned to must copy the values into the desired location in its
frame and deallocate excess stuff on the top of the stack.

More args are represented by a pointer to the block of values and a count.  The
function that originally created the more arg must allocate and deallocate this
stuff somehow.  In the case of a local call to a more arg entry, we can just
allocate it as a TN.  The external entry point for a more arg entry is more
magical.


The caller allocates the environment for the called function, stores the
arguments into it, and jumps to the function.  The caller makes the called
environment current, passing in the return OLD-CONT and PC as explicit arguments.

When returning values, the returner directly stores the return values into the
frame being returned to.  This works even though the caller doesn't know what
function it is returning to, since the same return locations are allocated in
all frames.

In a tail-recursive call, we can destructively modify the current frame and
jump right to the callee, rather than allocating a new frame.  We can do this
because TNBind globally allocates frame locations; all frames are the same size
and have the same TNs in the same place.


== Binding-Stack Format ==
\index{Binding stack format}
\comment<In a symbol chapter?>


The special binding stack is used to hold previous values of special variables
that have been bound.  It grows and shrinks with the depth of the binding
environment, as reflected in the control stack. This stack contains
symbol-value pairs, with only boxed Lisp objects present.

Each entry of the binding-stack consists of two boxed (32-bit) words.  Pushed
first is a pointer to the symbol being bound.  Pushed second is the symbol's
old value (any boxed item) that is to be restored when the binding stack is
popped.
]#


#[ Functions

Function calling is a way of life.

every function is a closure.  pointer to current closure is passed in ENV
unless it isn't (in local call may be elsewhere).

The description of the representation of functions and the function calling
conventions is a large part of the VM description, since:
    Function calling is one of the most complicated facilities provided by the
    VM.

    Everything that happens, happens in a function, so all parts of the system
    tend to get dragged in.


Aspects of function call:
    Control
    Environment CONT, ENV
    Argument/value passing
    Argument/value count dispatching




== Function Object Format ==
\label{Fn-Format}

The old notion of a "function object" is now broken down into four different
parts:
 + Function entry: A function entry is a structure that holds the information
   that we need to call a function.  This is the user visible function object.

 + Environment: The environment is stuff that a function needs when it runs.
   This includes constants computed at load time and variables closed over at run
   time.  Environment information may be allocated in the function entry structure
   after the required linkage information.

 + Entry information: This is information about a specific function entry that is
   occasionally referenced at run time, but need not be immediately accessible.
   Entry information will be either allocated in the function entry
   or in the environment that it points to.

 + Debug information: This is information about a function that isn't normally
   needed at run time.  Debug information can be found by poking around in
   environment objects.

See chapter \ref{control-conventions} for a description of how function objects
are used.


== Environment Object Sub-primitives ==

alloc-code ?
alloc-closure?


=== Debug Information Location ===

If present, debug information is stored immediately following any fixed
information in the environment object.  It may be necessary to chain up
multiple levels of environments to find the debug information.  The debug
information can be recognized because it is represented by a defstruct
structure.  See chapter \ref{debug-info} for a description of the debug
information.


== Function Calls ==
\index{function call}

\ccl supports three major calling conventions.  The convention used
depends on the amount of information available at compile time:
 + Local: Local call is used when the call and the called function are
   compiled at the same time.  Using the term "convention" to describe this
   call mechanism is somewhat of a misnomer, since the compiler can do
   whatever it wants.

 + Named: Named call is used when the call is to a global function whose name
   is known at compile time.

 + Anonymous: Anonymous call is used when the function called is unknown until
   run time.

    IR2 function call:

    Environment manipulation code is always emitted at the location of the Bind or
    Return node for a Lambda.

    Implicit args to functions in IR2:
      old-cont: cont to restore on return
      return-pc: pc to return to
      env: pointer to current closure (if heap)
      closure<n>: closed values for current closure (if stack)

    Other info needed for IR2 conversion of functions:
	base pointers for all heap closures consed by this function
	also have passing locs for each explicit arg
	return strategy (known or unknown) and return locs

    All arguments including implicit ones must have both a passing TN and a
    permanent TN.  Passing locs for let calls can be the actual TN that holds the
    variable in the case of local variables.  Set closure variables must still have
    a separate passing TN.

    If we know the values counts for the argument continuations, then we compile
    local mv-calls by moving the TNs for the values continuations into the argument
    passing locations.  Other mv-calls must be compiled using various hairy
    stack-hacking VOPs and unknown argument count call VOPs.

    For now, we will create the callee's frame just before the call, instead of
    creating it before the evaluation of the first argument.  If we created the
    environment early, then we would be able to move the argument values directly
    into the frame, instead of having to store them somewhere else for a while.
    The problem with early creation is that lifetime analysis gets confused because
    there is more than one instance of the same TN present simultaneously in the
    case where there are nested calls to the same function.

    It turns out that there isn't a problem with a simple self-call, because the TN
    in the called frame is really the "same" TN as the one in the current frame,
    due to the restricted way in which we use the passing TNs.

    We emit code for external entry points during IR2 conversion.  The external
    entry point is the place where we start running in a full call from a
    function-entry.  It does arg count checking and dispatching, moves the
    arguments into the passing locations for the for the lambda being called, and
    calls the lambda, moving the results into the standard locations if there
    aren't there already.

In IR2, the environment manipulation semantics of function call are decoupled
from the control semantics.  When allocating closure variables for a Let, it is
possible to do environment manipulation with only the normal sequential control
flow.  In the case of a Let call with the same environment, we neither
manipulate the environment nor transfer control; we merely initialize the
variables with Move VOPs.

If a local function returns a known number of values which is less than the
number expected by the caller, then additional code must be inserted at the
return site which sets the unused values to NIL.

The full function call mechanism must effectively be a subset of the local call
mechanism, since the two mechanisms must mesh at entry points and full function
calls.  A full call turns into some kind of full call VOP.  There are different
VOPs for calling named functions and closures.  We also have tail-recursive
full call VOPs.  Arguments are set up using Move VOPs, just as for local call.
The only difference is that the passing locations and conventions are
restricted to the standard ones.

The gory details of arg count checking and dispatching are buried in the
Function-Entry VOP, which takes a functional and a list of continuations, one
pointing to each external entry.


=== Local Call ===
\index{local call}

Named and anonymous call are called full calls, to distinguish them from
local call.  When making full calls, the compiler must make many worst-case
assumptions that aren't necessary in a local call.  The advantage of local
call is that the compiler can choose to use only those parts of the full
call sequence that are actually necessary.

In local call, we always know the function being called, so we never have
to do argument count checking, and can always use an immediate branch for
the control transfer.  If the function doesn't return to more than one
place, then can just use a simple branch, or even drop through.

The argument passing TNs may be allocated anywhere.  The caller allocates the
stack frame for the called function, moving any non-register arguments into the
passing locations in the callee's frame.

If we are calling a local function that doesn't always return the same
number of values, then we must use the same values returning mechanism that
is used in full call, but we don't have to use the standard registers.

A tail-recursive local call doesn't require any call VOP.  We just use Move
VOPs to put the arguments into the passing locations and then jump to the
start of the code for the function.  We don't have to do any stack hackery
since we use the same stack frame format for all the functions compiled at the
same time.  In many cases tail-recursive local calls can be entirely optimized
away, since they involve only some moves and a branch.  We preference the
argument values to the passing locations of the called function, making it
likely that no move will be necessary.  Often the control transfer can be done
by simply dropping through.

We have to do some funny stuff with local calls in order to get the lifetimes
for the passing locations right, since lifetime analysis skips directly from
the return point to the call point, ignoring the uses of the passing locations
in the called function.  Similarly, we pretend that a block ending in a return
has no successors.

call-local (arg*) "fun" => value
multiple-call-local (arg*) "fun" => start end val0 ... val<n>
    Call-Local is used for calls to local functions that are forced to use the
    unknown-values passing convention.  Value is the first return value
    register; we don't really do anything to it, but we specify it as a result
    to represent the assignment done by the calling function.

    Multiple-Call-Local is similar, but specifies all the values used by the
    unknown-values convention.  Default-Values may be used to receive a
    specific number of values.

known-call-local (arg*) "fun" => value*
    This VOP is used for local calls to functions where we can determine at
    compile time that the number of values returned is always the same.  In
    this case, we don't need to indicate the number of values, and can pass
    them in separate TNs.  The Values are the actual return locations.  We
    don't really do anything to the return values; we just specify them as
    results to represent the assignment done by the called function.

known-return (return-pc value*) "fun"
    This VOP is used for returning from local calls using the known return
    values convention.  The specified return Values are moved into the passing
    locations in the caller's frame.


If we know that the function we are calling is non-recursive, then we can
compile it much like a tail-recursive call.  We must have a call VOP to compute
the return PC, but we don't need to allocate a frame or save registers.  We
just set up the arguments in the frame and do the call.

We require simple functions to use the known-values convention.  It would be
possible to support unknown values, but it would potentially require BLT'ing
return values out of the frame and on to the top of the stack.  Supporting
unknown values would also require a bunch more VOPs, since we need different
call and return VOPs for simple call.

Known values return causes no problems, since the callee knows how many values
are wanted.  We store the values directly into the current frame, since it is
also the caller's frame.

known-call-simple () "fun" => return-pc
known-return-simple (return-pc) "fun"
    Similar to the non-simple VOPs, but don't allocate or deallocate frames,
    and assume that argument and value passing is done with explicit Move VOPs.


=== Full Call ===
\index{full call}

Both named and anonymous call are optimized for calls where the number of
arguments is known at compile time.  Unknown argument calls are a
pathological case of anonymous call; this case will be ignored in the main
discussion.  The difference between named and anonymous calls is in the
argument count dispatching mechanism.

Named call allows an arbitrary number of entry points, with start PCs at
arbitrary locations in the code vector.  The link-table mechanism described
below allows named calls to jump directly to the actual entry point without any
run-time argument count or type checking checking.

Anonymous call has a fixed number of entry points, with start PCs at fixed
locations in the code vector.  This allows calls to be made without knowing
what function is being called, but has more run-time overhead.  The object
called must be checked to be a valid function-entry object.  The entry PC must
be computed from the function entry, and argument count checking must be done
if there are more than three required or optional arguments.

Argument passing in full call is conceptually similar to local call, but the
caller can't allocate the entire frame for the callee, since it doesn't know
how much stack is needed.  Instead we allocate the frame in two parts.  The
caller only allocates the beginning of the frame, which contains the stack
arguments in fixed locations.  We leave the first <n> locations unused so that
the called function can move register more args onto the stack without having
to BLT down any stack arguments.

The place in the code where a full call jumps in is called an external entry
point.  The external entry point allocates the rest of the stack frame and then
does a local call to the actual entry-point function, fetching the arguments
from the standard passing locations.  Usually we can do a tail-recursive local
call.

There are two main cases where the call from the external entry point cannot be
tail-recursive:
 -- It is desirable to use the known-values convention for calling the
    entry-point function if the entry-point is used in other local calls
    (perhaps because of recursion).  In this case, the called function stores
    the return values back into the frame allocated by the external entry point
    and then returns back to it.  The external entry point must then return
    these values using the standard unknown-values convention.
 -- In a more-arg entry point we don't know how many stack arguments there are
    at the beginning of the frame, so we can't really use the frame allocated
    by the external entry point at all.  Instead we do a local call to the
    more-arg entry point, passing in a pointer to the first extra value.  When
    the function returns, we deallocate the crap on the stack and then return
    the values.  It is still o.k. to use the known-values return convention
    from the more-arg entry since the extra arg values are no longer needed by
    the time the returning function stores the return values back into the
    external entry point frame.


In full call we must always use the unknown-values convention for return.  The
first <n> values are passed in the standard argument registers.  The Old-Cont
register holds the Start of the values block and SP points to the End.


{small, fast} call (function arg0 ... arg<n>) "nargs" => value
{small, fast} call-named (arg0 ... arg<n>) "nargs" "name" => value
    FIX Call-Closure calls Function with the specified register arguments,
    returning the first value as the result.  "nargs" is the total number of
    arguments passed.  Only the register arguments actually passed should be
    specified as operands.

    Call-Named is similar, but calls a global function specified at compile
    time by "name".

{small, fast} tail-call (function pc arg0 ... arg<n>) "nargs"
{small, fast} tail-call-named (pc arg0 ... arg<n>) "nargs" "name"
    Similar to the standard call VOPs, but passes PC as the return PC, rather
    than returning to the call site.  These VOPs have no results since they
    don't return.

{small, fast} multiple-call (function arg0 ... arg<n>) "nargs"
                                    => start end val0 ... val<n>
{small, fast} multiple-call-named (arg0 ... arg<n>) "nargs" "name"
                                  => start end val0 ... val<n>
    These VOPs are similar to the standard call VOPs, but allow any number of
    values to be received by returning all the value passing registers as
    results.  A specific number of values may be received by using
    Default-Values.

call-unknown (function count arg0 ... arg<n>) => start end val0 ... val<n>
tail-call-unknown (function pc count arg0 ... arg<n>)
    Call a function with an unknown number of arguments.  Used for apply and
    hairy multiple-value-call.

Function-Entry () "function" => env return-pc old-cont arg*
    This marks the place where we jump into a component for an external
    entry point.  It represents whatever magic is necessary to do argument
    count checking and dispatching.  The external entry points for each
    argument count will be successors of the entry-vector block (might be in
    the same block if only one?)

    Function-Entry also represents argument passing by specifying the actual
    external passing locations as results, thus marking the beginning of their
    lifetimes.  All passing locations actually used by any entry point are
    specified as Args, including stack arguments.
   {XXX Do we really need this?  If we do, then we probably also need similar
    entry markers for local functions.  The lifetimes don't really need to be
    explicitly bounded, since an entry point is effectively "the end of the
    world."}


== Returning from a Function Call ==
\label(Return)
\index(Return)


return (return-pc value)
multiple-return (return-pc start end val0 ... val<n>)
    Return Value from the current function, jumping back to the location
    specified by Return-PC. {Perhaps allow to return any fixed, known number
    of values.}

    Multiple-Return is similar, but allows an arbitrary number of values to be
    returned.  End - Start is the total number of values returned.  Start
    points to the beginning of the block of return values, but the first <n>
    values val0 ... val<n> are actually returned in registers.

default-values (start end val0 ... val<n>) => val0 ... val<j>
    This VOP is used when we want to receive exactly J values.  If fewer than J
    values were supplied, then missing values are defaulted to NIL.  As a
    side-effect, this VOP pops off any returned stack values.


== Saving and Restoring Registers ==

We use a caller-saves convention.  The caller explicitly emits saving and
restoring code.  Tail-recursive calls don't need any register saving since
we never come back.
]#


#[ Non-local exits

=== Unwind Blocks ===
\index{Catch}
\index{Catch frames}

There is one aspect of the control stack format that is fixed, and which
concerns us at this level.  This is the format of the "frames" which mark the
destination of non-local exits, such as for BLOCK and CATCH.  These frames are
collectively known as unwind blocks.  The basic unwind block is used for
lexical exists such as BLOCK, and for UNWIND-PROTECT.  Its format is the
following:
\begin{verbatim}
0   Pointer to current unwind-protect.
1   Control stack context to restore on entry.
2   PC to enter at.
\end{verbatim}

The unwind block for CATCH is identical except for additional cells
containing the catch tag and previous catch.
\begin{verbatim}
0   Pointer to current unwind-protect.
1   Control stack context to restore on entry.
2   PC to enter at.
3   Catch tag.
4   Previous catch.
\end{verbatim}

The conventions used to manipulate unwind blocks are described in chapter
\ref{Control-Conventions}.



== Non-Local Exits ==
\label{Catch}
\index{Catch}
\index{Throw}
\index{Unwinding}
\index{Unwind-Protect}
\index{Non-Local Exits}

In the normal flow of control, each function that is called executes until it
reaches a return point; under these conditions no special effort is needed to
restore the environment as long as each function undoes any change that it
makes to the dynamic state before it returns.  When we make a non-local
transfer, we skip a potentially arbitrary collection of these cleanup actions.
Since we cannot in general know what changes have been made to the dynamic
environment below us on the stack, we must restore a snapshot of the dynamic
environment at the re-entry point.

We represent the closed continuation by the pointer to the unwind-block for the
reentry point.  At the exit point, we just pass this stack pointer to the
Unwind VOP, which deals with processing any unwind-protects.  When Unwind is
done, it grabs the re-entry PC out of the location at the stack pointer and
jumps in.

Catch and Unwind-Protect work in pretty much the same way.  We make a stack TN
to hold the catch frame or whatever, allocate TNs in them to represent the
slots, and then initialize them.  The frame can be explicitly linked in by TN
manipulations, since the active catch and whatnot are represented by TNs.
Since allocation of the frame is decoupled from linking and unlinking, some of
this stuff could be moved out of loops.  We will need a VOP for loading the PC
for an arbitrary continuation so that we can set up the reentry PC.  This can
be done using the Call VOP.  Using a call instruction is probably a good way to
get a PC on most architectures anyway.

These TNs are allocated by Pack like any others; we use special alloc and
dealloc VOPs to delimit the aggregate lifetimes.

In the non-local case, the the Block, Catch and Unwind-Protect special forms
are implemented using unwind blocks.  The unwind blocks are built by move
operations emitted inline by the compiler.  The compiler adds and removes
catches and unwind protects by explicit moves to the locations that hold the
current catch and unwind protect blocks.  The entry PC is loaded using the Call
VOP.

The Unwind miscop is the basis non-local exits.  It takes the address of an
unwind block and processes unwind-protects until the current unwind-protect is
the one recorded in the unwind block, then jumps in at the entry in the unwind
block.  The entry for the unwind block is responsible for restoring any state
other than the current unwind-protect.

Unwind is used directly to implement non-local Return-From.  The address of the
unwind block is stored in a closure variable.

Catch just does a scan up the chain of Catch blocks, starting at the current
catch.  When it finds the right one, it calls unwind on it.

Unwind-protects are represented by unwind blocks linked into the current
unwind-protect chain.  The cleanup code is entered just like any other any
other unwind entry.  As before, the entry is responsible for establishing the
correct dynamic environment for the cleanup code.  The target unwind block is
passed in some non-argument register.  When the cleanup code is done, it
just calls Unwind with the block passed in.  The cleanup code must be careful
not to trash the argument registers or CS, since there may be multiple values
lurking out there.

With Catch/Throw, we always use the variable values return value passing convention,
since we don't know how many values the catch wants.  With Block/Return-From,
we can do whatever we want, since the returner and receiver know each other.

If a Block or Catch receives stack values, it must call a VOP that BLT's the
values down the stack, squeezing out any intermediate crud.


unwind (context)
throw (tag)
    Unwind does a non-local exit, unwinding to the place indicated by Context.
    Context is a pointer to a block of storage allocated on the control stack,
    containing the entry PC, current environment and current unwind-protect.
    We scan up the stack, processing unwind-protects until we reach the entry
    point.  The values being returned are passed in the standard locations.
    Throw is similar, but does a dynamic lookup for the Tag to determine what
    context to unwind to.
]#
