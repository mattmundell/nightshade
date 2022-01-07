;;; This file isolates all the backend specific data so that we can compile
;;; and use different backends.

(in-package "C")

(export '(*backend* *target-backend* *native-backend* backend
	  backend-name backend-version backend-fasl-file-type
	  backend-fasl-file-implementation backend-fasl-file-version
	  backend-register-save-penalty backend-byte-order
	  backend-any-primitive-type backend-info-environment
	  backend-instruction-formats backend-instruction-flavors
	  backend-assembler-resources backend-special-arg-types
	  backend-disassem-params backend-internal-errors
	  backend-assembler-params backend-page-size
	  ;;
	  ;; The various backends need to call these support routines
	  def-vm-support-routine make-stack-pointer-tn primitive-type
	  primitive-type-of emit-nop location-number))


;;;; VM support routine stuff.

(eval-when (compile eval)

(defmacro def-vm-support-routines (&rest routines)
  `(progn
     (eval-when (compile load eval)
       (defparameter vm-support-routines ',routines))
     (defstruct (vm-support-routines
		 (:print-function %print-vm-support-routines))
       ,@(mapcar #'(lambda (routine)
		     `(,routine nil :type (or function null)))
		 routines))
     ,@(mapcar
	#'(lambda (name)
	    `(defun ,name (&rest args)
	       (apply (or (,(symbolicate "VM-SUPPORT-ROUTINES-" name)
			   (backend-support-routines *backend*))
			  (error "Machine specific support routine ~S ~
				  undefined for ~S"
				 ',name *backend*))
		      args)))
	routines)))

); eval-when

(def-vm-support-routines
  ;; From VM.LISP
  immediate-constant-sc
  location-print-name

  ;; From PRIMTYPE.LISP
  primitive-type-of
  primitive-type

  ;; From C-CALL.LISP
  make-call-out-tns

  ;; From CALL.LISP
  standard-argument-location
  make-return-pc-passing-location
  make-old-fp-passing-location
  make-old-fp-save-location
  make-return-pc-save-location
  make-argument-count-location
  make-nfp-tn
  make-stack-pointer-tn
  make-number-stack-pointer-tn
  make-unknown-values-locations
  select-component-format

  ;; From NLX.LISP
  make-nlx-sp-tn
  make-dynamic-state-tns
  make-nlx-entry-argument-start-location

  ;; From SUPPORT.LISP
  generate-call-sequence
  generate-return-sequence

  ;; For use with scheduler.
  emit-nop
  location-number)

(defprinter vm-support-routines)

(defmacro def-vm-support-routine (name ll &body body)
  (or (member (intern (string name) (find-package "C"))
	      vm-support-routines)
      (warn "Unknown VM support routine: ~A" name))
  (let ((local-name (symbolicate (backend-name *target-backend*) "-" name)))
    `(progn
       (defun ,local-name ,ll ,@body)
       (setf (,(intern (concatenate 'simple-string
				    "VM-SUPPORT-ROUTINES-"
				    (string name))
		       (find-package "C"))
	      (backend-support-routines *target-backend*))
	     #',local-name))))


;;;; The backend structure.

(defstruct (backend
	    (:print-function %print-backend))
  ;; The name of this backend.  Something like ``PMAX''
  (name nil)

  ;; The version string for this backend.
  ;; Something like ``DECstation 3100/Mach 0.0''
  (version nil)

  ;; Information about fasl files for this backend.
  (fasl-file-type nil)
  (fasl-file-implementation nil)
  (fasl-file-version nil)

  ;; The VM support routines.
  (support-routines (make-vm-support-routines) :type vm-support-routines)

  ;; The number of references that a TN must have to offset the overhead of
  ;; saving the TN across a call.
  (register-save-penalty 0)

  ;; The byte order of the target machine.  Should either be :big-endian
  ;; which has the MSB first (RT) or :little-endian which has the MSB last
  ;; (VAX).
  (byte-order nil :type (or null (member :little-endian :big-endian)))

  ;; Translates from SC numbers to SC info structures.  SC numbers are always
  ;; used instead of names at run time, so changing this vector changes all the
  ;; references.
  (sc-numbers (make-array sc-number-limit :initial-element nil)
	      :type sc-vector)

  ;; A list of all the SBs defined, so that we can easily iterate over them.
  (sb-list () :type list)

  ;; Translates from template names to template structures.
  (template-names (make-hash-table :test #'eq) :type hash-table)

  ;; Hashtable from SC and SB names the corresponding structures.  The META
  ;; versions are only used at meta-compile and load times, so the defining
  ;; macros can change these at meta-compile time without breaking the
  ;; compiler.
  (sc-names (make-hash-table :test #'eq) :type hash-table)
  (sb-names (make-hash-table :test #'eq) :type hash-table)
  (meta-sc-names (make-hash-table :test #'eq) :type hash-table)
  (meta-sb-names (make-hash-table :test #'eq) :type hash-table)

  ;; Like *SC-Numbers*, but is updated at meta-compile time.
  (meta-sc-numbers (make-array sc-number-limit :initial-element nil)
		   :type sc-vector)

  ;; Translates from primitive type names to the corresponding primitive-type
  ;; structure.
  (primitive-type-names (make-hash-table :test #'eq) :type hash-table)

  ;; Establishes a convenient handle on primitive type unions, or whatever.
  ;; These names can only be used as the :arg-types or :result-types for VOPs
  ;; and can map to anything else that can be used as :arg-types or
  ;; :result-types (e.g. :or, :constant).
  (primitive-type-aliases (make-hash-table :test #'eq) :type hash-table)

  ;; Meta-compile time translation from names to primitive types.
  (meta-primitive-type-names (make-hash-table :test #'eq) :type hash-table)

  ;; The primitive type T is somewhat magical, in that it is the only
  ;; primitive type that overlaps with other primitive types.  An object
  ;; of primitive-type T is in the canonical descriptor (boxed or pointer)
  ;; representation.
  ;;
  ;; We stick the T primitive-type in a variable so that people who have to
  ;; special-case it can get at it conveniently.  This is done by the machine
  ;; specific VM definition, since the DEF-PRIMITIVE-TYPE for T must specify
  ;; the SCs that boxed objects can be allocated in.
  (any-primitive-type nil :type (or null primitive-type))

  ;; Hashtable translating from VOP names to the corresponding VOP-Parse
  ;; structures.  This information is only used at meta-compile time.
  (parsed-vops (make-hash-table :test #'eq) :type hash-table)

  ;; The backend specific aspects of the info environment.
  (info-environment nil :type list)

  ;; Support for the assembler.
  (instruction-formats (make-hash-table :test #'eq) :type hash-table)
  (instruction-flavors (make-hash-table :test #'equal) :type hash-table)
  (special-arg-types (make-hash-table :test #'eq) :type hash-table)
  (assembler-resources nil :type list)

  ;; The backend specific features list, if any.  During a compilation,
  ;; *features* is bound to *features* - misfeatures + features.
  (%features nil :type list)
  (misfeatures nil :type list)

  ;; Disassembler information.
  (disassem-params nil :type t)

  ;; Mappings between CTYPE structures and the corresponding predicate.
  ;; The type->predicate mapping hash is an alist because there is no
  ;; such thing as a type= hash table.
  (predicate-types (make-hash-table :test #'eq) :type hash-table)
  (type-predicates nil :type list)

  ;; Vector of the internal errors defined for this backend, or NIL if
  ;; they haven't been installed yet.
  (internal-errors nil :type (or simple-vector null))

  ;; Assembler parameters.
  (assembler-params nil :type t)

  ;; The maximum number of bytes per page on this system.  Used by genesis.
  (page-size 0 :type index))

(defprinter backend
  name)

(defvar *native-backend* (make-backend)
  "The backend for the machine we are running on. Do not change this.")
(defvar *target-backend* *native-backend*
  "The backend we are attempting to compile.")
(defvar *backend* *native-backend*
  "The backend we are using to compile with.")


;;;; Other utility functions for fiddling with the backend.

(export '(backend-features target-featurep backend-featurep native-featurep))

(defun backend-features (backend)
  "Compute the *FEATURES* list to use with BACKEND."
  (union (backend-%features backend)
	 (set-difference *features*
			 (backend-misfeatures backend))))

(defun target-featurep (feature)
  "Same as EXT:FEATUREP, except use the features found in *TARGET-BACKEND*."
  (let ((*features* (backend-features *target-backend*)))
    (featurep feature)))

(defun backend-featurep (feature)
  "Same as EXT:FEATUREP, except use the features found in *BACKEND*."
  (let ((*features* (backend-features *backend*)))
    (featurep feature)))

(defun native-featurep (feature)
  "Same as EXT:FEATUREP, except use the features found in *NATIVE-BACKEND*."
  (let ((*features* (backend-features *native-backend*)))
    (featurep feature)))

;;; NEW-BACKEND
;;;
;;; Utility for creating a new backend structure for use with cross
;;; compilers.
;;;
(defun new-backend (name features misfeatures)
  ;; If VM names a different package, rename that package so that VM doesn't
  ;; name it.
  (let ((pkg (find-package "VM")))
    (when pkg
      (let ((pkg-name (package-name pkg)))
	(unless (string= pkg-name name)
	  (rename-package pkg pkg-name
			  (remove "VM" (package-nicknames pkg)
				  :test #'string=))
	  (unuse-package pkg "C")))))
  ;; Make sure VM names our package, creating it if necessary.
  (let* ((pkg (or (find-package name)
		  (make-package name :nicknames '("VM"))))
	 (nicknames (package-nicknames pkg)))
    (or (member "VM" nicknames :test #'string=)
	(rename-package pkg name (cons "VM" nicknames)))
    ;; And make sure we are using the necessary packages.
    (use-package '("C-CALL" "ALIEN-INTERNALS" "ALIEN" "BIGNUM" "UNIX"
		   "LISP" "KERNEL" "EXTENSIONS" "SYSTEM" "C" "NEW-ASSEM")
		 pkg))
  ;; Make sure the native info env list is stored in *native-backend*
  (or (backend-info-environment *native-backend*)
      (setf (backend-info-environment *native-backend*) *info-environment*))
  ;; Cons up a backend structure, filling in the info-env and features slots.
  (let ((backend (make-backend
		  :name name
		  :info-environment
		  (cons (make-info-environment
			 :name
			 (concatenate 'string name " backend"))
			(remove-if #'(lambda (name)
				       (let ((len (length name)))
					 (and (> len 8)
					      (string= name " backend"
						       :start1 (- len 8)))))
				   *info-environment*
				   :key #'info-env-name))
		  :%features features
		  :misfeatures misfeatures)))
    (setf *target-backend* backend)
    (define-standard-type-predicates)
    backend))


#[ Compiler Retargeting
    XXX

    In general, it is a danger sign if a generator references a TN that isn't an
    operand or temporary, since lifetime analysis hasn't been done for that use.
    We are doing weird stuff for the old-cont and return-pc passing locations,
    hoping that the conflicts at the called function have the desired effect.
    Other stuff?  When a function returns unknown values, we don't reference the
    values locations when a single-value return is done.  But nothing is live at a
    return point anyway.


    Have a way for template conversion to special-case constant arguments?
    How about:
	If an arg restriction is (:satisfies [<predicate function>]), and the
	corresponding argument is constant, with the constant value satisfying the
	predicate, then (if any other restrictions are satisfied), the template
	will be emitted with the literal value passed as an info argument.  If the
	predicate is omitted, then any constant will do.

	We could sugar this up a bit by allowing (:member <object>*) for
	(:satisfies (lambda (x) (member x '(<object>*))))

    We could allow this to be translated into a Lisp type by adding a new Constant
    type specifier.  This could only appear as an argument to a function type.
    To satisfy (Constant <type>), the argument must be a compile-time constant of
    the specified type.  Just Constant means any constant (i.e. (Constant *)).
    This would be useful for the type constraints on ICR transforms.


    Constant TNs: we count on being able to indirect to the leaf, and don't try to
    wedge the information into the offset.  We set the FSC to an appropriate
    immediate SC.

	Allow "more operands" to VOPs in define-vop.  You can't do much with the
	more operands: define-vop just fills in the cost information according to
	the loading costs for a SC you specify.  You can't restrict more operands,
	and you can't make local preferences.  In the generator, the named variable
	is bound to the TN-ref for the first extra operand.  This should be good
	enough to handle all the variable arg VOPs (primarily function call and
	return).  Usually more operands are used just to get TN lifetimes to work
	out; the generator actually ignores them.

	Variable-arg VOPs can't be used with the VOP macro.  You must use VOP*.
	VOP* doesn't do anything with these extra operand except stick them on the
	ends of the operand lists passed into the template.  VOP* is often useful
	within the convert functions for non-VOP templates, since it can emit a VOP
	using an already prepared TN-Ref list.


	It is pretty basic to the whole primitive-type idea that there is only one
	primitive-type for a given lisp type.  This is really the same as saying
	primitive types are disjoint.  A primitive type serves two somewhat
	unrelated purposes:
	 -- It is an abstraction a Lisp type used to select type specific
	    operations.  Originally kind of an efficiency hack, but it lets a
	    template's type signature be used both for selection and operand
	    representation determination.
	 -- It represents a set of possible representations for a value (SCs).  The
	    primitive type is used to determine the legal SCs for a TN, and is also
	    used to determine which type-coercion/move VOP to use.

There are basically three levels of target dependence:

 * Code in the "front end" (before VMR conversion) deals only with Lisp
   semantics, and is totally target independent.

 * Code after VMR conversion and before code generation depends on the VM,
   but should work with little modification across a wide range of
   "conventional" architectures.

 * Code generation depends on the machine's instruction set and other
   implementation details, so it has to be redone for each implementation.
   Most of the work here is in defining the translation into assembly code
   of all the supported VOPs.

[ Storage bases and classes    ]
[ Type system parameterization ]
[ VOP Definition               ]
[ Assembler Retargeting        ]
[ Writing Assembly Code        ]
[ Required VOPS                ]
[ Standard Primitives          ]
[ Customizing VMR Conversion   ]
]#

#[ Assembler Retargeting
[FIX]
]#


#[ Required VOPS

Note: the move VOP cannot have any wired temps.  (Move-Argument also?)  This is
so we can move stuff into wired TNs without stepping on our toes.


We create set closure variables using the Value-Cell VOP, which takes a value
and returns a value cell containing the value.  We can basically use this
instead of a Move VOP when initializing the variable.  Value-Cell-Set and
Value-Cell-Ref are used to access the value cell.  We can have a special effect
for value cells so that value cells references can be discovered to be common
subexpressions or loop invariants.


Represent unknown-values continuations as (start, count).  Unknown values
continuations are always outside of the current frame (on stack top).  Within a
function, we always set up and receive values in the standard passing
locations.  If we receive stack values, then we must BLT them down to the start
of our frame, filling in any unsupplied values.  If we generate unknown values
(i.e. PUSH-VALUES), then we set the values up in the standard locations, then
BLT them to stack top.  When doing a tail-return of MVs, we just set them up in
the standard locations and decrement SP: no BLT is necessary.

Unknown argument call (MV-CALL) takes its arguments on stack top (is given a
base pointer).  If not a tail call, then we just set the arg pointer to the
base pointer and call.  If a tail call, we must BLT the arguments down to the
beginning of the current frame.


Implement more args by BLT'ing the more args *on top* of the current frame.
This solves two problems:
 -- Any register more arguments can be made uniformly accessibly by copying
    them into memory.
        We can't store the registers in place, since the
        beginning of the frame gets double use for storing the old-cont, return-pc
        and env.
 -- It solves the deallocation problem: the arguments will be deallocated when
    the frame is returned from or a tail full call is done out of it.  So
    keyword args will be properly tail-recursive without any special mechanism
    for squeezing out the more arg once the parsing is done.  Note that a tail
    local call won't blast the more arg, since in local call the callee just
    takes the frame it is given (in this case containing the more arg).

More args in local call???  Perhaps we should not attempt local call conversion
in this case.  We already special-case keyword args in local call.  It seems
that the main importance of more args is primarily related to full call: it is
used for defining various kinds of frobs that need to take arbitrary arguments:
 -- Keyword arguments
 -- Interpreter stubs
 -- "Pass through" applications such as dispatch functions

Given the marginal importance of more args in local call, it seems unworth
going to any implementation difficulty.  In fact, it seems that it would cause
complications both at the VMR level and also in the VM definition.  This being
the case, we should flush it.

== Function Call ==

=== Registers and frame format ===

These registers are used in function call and return:

A0..A{\it n}
    In full call, the first three arguments.  In unknown values return, the
    first three return values.

CFP
    The current frame pointer.  In full call, this initially points to a
    partial frame large enough to hold the passed stack arguments (zero-length
    if none).

CSP
    The current control stack top pointer.

OCFP
    In full call, the passing location for the frame to return to.

    In unknown-values return of other than one value, the pointer to returned
    stack values.  In such a return, OCFP is always initialized to point to
    the frame returned from, even when no stack values are returned.  This
    allows OCFP to be used to restore CSP.

LRA
    In full call, the passing location for the return PC.

NARGS
    In full call, the number of arguments passed.  In unknown-values return of
    other than one value, the number of values returned.

=== Full call ===

What is our usage of CFP, OCFP and CSP?

It is an invariant that CSP always points after any useful information so that
at any time an interrupt can come and allocate stuff in the stack.

TR call is also a constraint: we can't deallocate the caller's frame before the
call, since it holds the stack arguments for the call.

What we do is have the caller set up CFP, and have the callee set CSP to CFP
plus the frame size.  The caller leaves CSP alone: the callee is the one who
does any necessary stack deallocation.

In a TR call, we don't do anything: CFP is left as CFP, and CSP points to the
end of the frame, keeping the stack arguments from being trashed.

In a normal call, CFP is set to CSP, causing the callee's frame to be allocated
after the current frame.

=== Unknown values return ===

The unknown values return convention is always used in full call, and is used
in local call when the compiler either can't prove that a fixed number of
values are returned, or decides not to use the fixed values convention to allow
tail-recursive XEP calls.

The unknown-values return convention has variants: single value and variable
values.  We make this distinction to optimize the important case of a returner
whose knows exactly one value is being returned.  Note that it is possible to
return a single value using the variable-values convention, but it is less
efficient.

We indicate single-value return by returning at the return-pc+4; variable value
return is indicated by returning at the return PC.

Single-value return makes only the following guarantees:
    A0 holds the value returned.
    CSP has been reset: there is no garbage on the stack.

In variable value return, more information is passed back:
    A0..A2 hold the first three return values.  If fewer than three values are
    returned, then the unused registers are initialized to NIL.

    OCFP points to the frame returned from.  Note that because of our
    tail-recursive implementation of call, the frame receiving the values is
    always immediately under the frame returning the values.  This means that
    we can use OCFP to index the values when we access them, and to restore
    CSP when we want to discard them.

    NARGS holds the number of values returned.

    CSP is always (+ OCFP (* NARGS 4)), i.e. there is room on the stack
    allocated for all returned values, even if they are all actually passed in
    registers.


=== External Entry Points ===

Things that need to be done on XEP entry:
 1] Allocate frame
 2] Move more arg above the frame, saving context
 3] Set up env, saving closure pointer if closure
 4] Move arguments from closure to local home
    Move old-cont and return-pc to the save locations
 5] Argument count checking and dispatching

XEP VOPs:

Allocate-Frame
Copy-More-Arg <nargs-tn> 'fixed {in a3} => <context>, <count>
Setup-Environment
Setup-Closure-Environment => <closure>
Verify-Argument-Count <nargs-tn> 'count {for fixed-arg lambdas}
Argument-Count-Error <nargs-tn> {Drop-thru on hairy arg dispatching}
Use fast-if-=/fixnum and fast-if-</fixnum for dispatching.

Closure vops:
make-closure <fun entry> <slot count> => <closure>
closure-init <closure> <values> 'slot


Things that need to be done on all function entry:
 -- Move arguments to the variable home (consing value cells as necessary)
 -- Move environment values to the local home
 -- Move old-cont and return-pc to the save locations


== Calls ==

Calling VOP's are a cross product of the following sets (with some members
missing):
   Return values
      multiple (all values)
      fixed (calling with unknown values conventions, wanting a certain
             number.)
      known (only in local call where caller/callee agree on number of
      	     values.)
      tail (doesn't return but does tail call)
   What function
      local
      named (going through symbol, like full but stash fun name for error sys)
      full (have a function)
   Args
      fixed (number of args are known at compile-time)
      variable (MULTIPLE-VALUE-CALL and APPLY)

Note on all jumps for calls and returns that we want to put some instruction
in the jump's delay slot(s).

Register usage at the time of the call:

LEXENV
   This holds the lexical environment to use during the call if it's a closure,
   and it is undefined otherwise.

CNAME
   This holds the symbol for a named call and garbage otherwise.

OCFP
   This holds the frame pointer, which the system restores upon return.  The
   callee saves this if necessary; this is passed as a pseudo-argument.

A0 ... An
   These holds the first n+1 arguments.

NARGS
   This holds the number of arguments, as a fixnum.

LRA
   This holds the lisp-return-address object which indicates where to return.
   For a tail call, this retains its current value.  The callee saves this
   if necessary; this is passed as a pseudo-argument.

CODE
   This holds the function object being called.

CSP
   The caller ignores this.  The callee sets it as necessary based on CFP.

CFP
   This holds the callee's frame pointer.  Caller sets this to the new frame
   pointer, which it remembered when it started computing arguments; this is
   CSP if there were no stack arguments.  For a tail call CFP retains its
   current value.

NSP
   The system uses this within a single function.  A function using NSP must
   allocate and deallocate before returning or making a tail call.

Register usage at the time of the return for single value return, which
goes with the unknown-values convention the caller used.

A0
   The holds the value.

CODE
   This holds the lisp-return-address at which the system continues executing.

CSP
   This holds the CFP.  That is, the stack is guaranteed to be clean, and there
   is no code at the return site to adjust the CSP.

CFP
   This holds the OCFP.

Additional register usage for multiple value return:

NARGS
   This holds the number of values returned.

A0 ... An
   These holds the first n+1 values, or NIL if there are less than n+1 values.

CSP
   Returner stores CSP to hold its CFP + NARGS * <address units per word>

OCFP
   Returner stores this as its CFP, so the returnee has a handle on either
   the start of the returned values on the stack.


ALLOCATE FULL CALL FRAME.

If the number of call arguments (passed to the VOP as an info argument)
indicates that there are stack arguments, then it makes some callee frame for
arguments:
   VOP-result <- CSP
   CSP <- CSP + value of VOP info arg times address units per word.

In a call sequence, move some arguments to the right places.

There's a variety of MOVE-ARGUMENT VOP's.

FULL CALL VOP'S
(variations determined by whether it's named, it's a tail call, there
is a variable arg count, etc.)

  if variable number of arguments
    NARGS <- (CSP - value of VOP argument) shift right by address units per word.
    A0...An <- values off of VOP argument (just fill them all)
  else
    NARGS <- value of VOP info argument (always a constant)

  if tail call
    OCFP <- value from VOP argument
    LRA <- value from VOP argument
    CFP stays the same since we reuse the frame
    NSP <- NFP
  else
    OCFP <- CFP
    LRA <- compute LRA by adding an assemble-time determined constant to
    	   CODE.
    CFP <- new frame pointer (remembered when starting to compute args)
           This is CSP if no stack args.
    when (current-nfp-tn VOP-self-pointer)
      stack-temp <- NFP

  if named
    CNAME <- function symbol name
    the-fun <- function object out of symbol

  LEXENV <- the-fun (from previous line or VOP argument)
  CODE <- function-entry (the first word after the-fun)
  LIP <- calc first instruction addr (CODE + constant-offset)
  jump and run off temp

  <emit Lisp return address data-block>
  <default and move return values OR receive return values>
  when (current-nfp-tn VOP-self-pointer)
    NFP <- stack-temp

Callee:

XEP-ALLOCATE-FRAME
  emit function header (maybe initializes offset back to component start,
  			but other pointers are set up at load-time.  Pads
			to dual-word boundary.)
  CSP <- CFP + compile-time determined constant (frame size)
  if the function uses the number stack
    NFP <- NSP
    NSP <- NSP + compile-time determined constant (number stack frame size)

SETUP-ENVIRONMENT
(either use this or the next one)

CODE <- CODE - assembler-time determined offset from function-entry back to
	       the code data-block address.

SETUP-CLOSURE-ENVIRONMENT
(either use this or the previous one)
After this the CLOSURE-REF VOP can reference closure variables.

VOP-result <- LEXENV
CODE <- CODE - assembler-time determined offset from function-entry back to
	       the code data-block address.

Return VOP's
RETURN and RETURN-MULTIPLE are for the unknown-values return convention.
For some previous caller this is either it wants n values (and it doesn't
know how many are coming), or it wants all the values returned (and it
doesn't know how many are coming).


RETURN
(known fixed number of values, used with the unknown-values convention
 in the caller.)
When compiler invokes VOP, all values are already where they should be;
just get back to caller.

when (current-nfp-tn VOP-self-pointer)
  ;; The number stack grows down in memory.
  NSP <- NFP + number stack frame size for calls within the currently
                  compiling component
	       times address units per word
CODE <- value of VOP argument with LRA
if VOP info arg is 1 (number of values we know we're returning)
  CSP <- CFP
  LIP <- calc target addr
          (CODE + skip over LRA header word + skip over address units per branch)
	  (The branch is in the caller to skip down to the MV code.)
else
  NARGS <- value of VOP info arg
  nil out unused arg regs
  OCFP <- CFP  (This indicates the start of return values on the stack,
  		but you leave space for those in registers for convenience.)
  CSP <- CFP + NARGS * address-units-per-word
  LIP <- calc target addr (CODE + skip over LRA header word)
CFP <- value of VOP argument with OCFP
jump and run off LIP

RETURN-MULTIPLE
(unknown number of values, used with the unknown-values convention in
 the caller.)
When compiler invokes VOP, it gets TN's representing a pointer to the
values on the stack and how many values were computed.

when (current-nfp-tn VOP-self-pointer)
  ;; The number stack grows down in memory.
  NSP <- NFP + number stack frame size for calls within the currently
                  compiling component
	       times address units per word
NARGS <- value of VOP argument
copy the args to the beginning of the current (returner's) frame.
   Actually some go into the argument registers.  When putting the rest at
   the beginning of the frame, leave room for those in the argument registers.
CSP <- CFP + NARGS * address-units-per-word
nil out unused arg regs
OCFP <- CFP  (This indicates the start of return values on the stack,
	      but you leave space for those in registers for convenience.)
CFP <- value of VOP argument with OCFP
CODE <- value of VOP argument with LRA
LIP <- calc target addr (CODE + skip over LRA header word)
jump and run off LIP


Returnee
The call VOP's call DEFAULT-UNKNOWN-VALUES or RECEIVE-UNKNOWN-VALUES after
spitting out transfer control to get stuff from the returner.

DEFAULT-UNKNOWN-VALUES
(We know what we want and we got something.)
If returnee wants one value, it never does anything to deal with a shortage
of return values.  However, if start at PC, then it has to adjust the stack
pointer to dump extra values (move OCFP into CSP).  If it starts at PC+N,
then it just goes along with the "want one value, got it" case.
If the returnee wants multiple values, and there's a shortage of return
values, there are two cases to handle.  One, if the returnee wants fewer
values than there are return registers, and we start at PC+N, then it fills
in return registers A1..A<desired values necessary>; if we start at PC,
then the returnee is fine since the returning conventions have filled in
the unused return registers with nil, but the returnee must adjust the
stack pointer to dump possible stack return values (move OCFP to CSP).
Two, if the returnee wants more values than the number of return registers,
and it starts at PC+N (got one value), then it sets up returnee state as if
an unknown number of values came back:
   A0 has the one value
   A1..An get nil
   NARGS gets 1
   OCFP gets CSP, so general code described below can move OCFP into CSP
If we start at PC, then branch down to the general "got k values, wanted n"
code which takes care of the following issues:
   If k < n, fill in stack return values of nil for shortage of return
      values and move OCFP into CSP
   If k >= n, move OCFP into CSP
This also restores CODE from LRA by subtracting an assemble-time constant.

RECEIVE-UKNOWN-VALUES
(I want whatever I get.)
We want these at the end of our frame.  When the returnee starts starts at
PC, it moves the return value registers to OCFP..OCFP~[An] ignoring where
the end of the stack is and whether all the return value registers had
values.  The returner left room on the stack before the stack return values
for the register return values.  When the returnee starts at PC+N, bump CSP
by 1 and copy A0 there.
This also restores CODE from LRA by subtracting an assemble-time constant.


Local call

There are three flavors:
   1] KNOWN-CALL-LOCAL
      Uses known call convention where caller and callee agree where all
      the values are, and there's a fixed number of return values.
   2] CALL-LOCAL
      Uses the unknown-values convention, but we expect a particular
      number of values in return.
   3] MULTIPLE-CALL-LOCAL
      Uses the unknown-values convention, but we want all values returned.

ALLOCATE-FRAME

If the number of call arguments (passed to the VOP as an info argument)
indicates that there are stack arguments, then it makes some callee frame for
arguments:
   VOP-result1 <- CSP
   CSP <- CSP + control stack frame size for calls within the currently
   		   compiling component
   		times address units per word.
   when (callee-nfp-tn <VOP info arg holding callee>)
     ;; The number stack grows down.
     ;; May have to round to dual-word boundary if machines C calling
     ;;    conventions demand this.
     NSP <- NSP - number stack frame size for calls within the currently
     		     compiling component
		  times address units per word
     VOP-result2 <- NSP

KNOWN-CALL-LOCAL, CALL-LOCAL, MULTIPLE-CALL-LOCAL
KNOWN-CALL-LOCAL has no need to affect CODE since CODE is the same for the
caller/returnee and the returner.  This uses KNOWN-RETURN.
With CALL-LOCAL and MULTIPLE-CALL-LOCAL, the caller/returnee must fixup
CODE since the callee may do a tail full call.  This happens in the code
emitted by DEFAULT-UNKNOWN-VALUES and RECEIVE-UNKNOWN-VALUES.  We use these
return conventions since we don't know what kind of values the returner
will give us.  This could happen due to a tail full call to an unknown
function, or because the callee had different return points that returned
various numbers of values.

when (current-nfp-tn VOP-self-pointer)   ;Get VOP self-pointer with
					 ;DEFINE-VOP switch :vop-var.
  stack-temp <- NFP
CFP <- value of VOP arg
when (callee-nfp-tn <VOP info arg holding callee>)
  <where-callee-wants-NFP-tn>  <-  value of VOP arg
<where-callee-wants-LRA-tn>  <-  compute LRA by adding an assemble-time
				 determined constant to CODE.
jump and run off VOP info arg holding start instruction for callee

<emit Lisp return address data-block>
<case call convention
  known: do nothing
  call: default and move return values
  multiple: receive return values
>
when (current-nfp-tn VOP-self-pointer)
  NFP <- stack-temp

KNOWN-RETURN

CSP <- CFP
when (current-nfp-tn VOP-self-pointer)
  ;; number stack grows down in memory.
  NSP <- NFP + number stack frame size for calls within the currently
                  compiling component
	       times address units per word
LIP <- calc target addr (value of VOP arg + skip over LRA header word)
CFP <- value of VOP arg
jump and run off LIP
]#


#[ Customizing VMR Conversion

Another way in which different implementations differ is in the relative cost
of operations.  On machines without an integer multiply instruction, it may be
desirable to convert multiplication by a constant into shifts and adds, while
this is surely a bad idea on machines with hardware support for multiplication.
Part of the tuning process for an implementation will be adding implementation
dependent transforms and disabling undesirable standard transforms.

When practical, ICR transforms should be used instead of VMR generators, since
transforms are more portable and less error-prone.  Note that the Lisp code
need not be implementation independent: it may contain all sorts of
sub-primitives and similar stuff.  Generally a function should be implemented
using a transform instead of an VMR translator unless it cannot be implemented
as a transform due to being totally evil or it is just as easy to implement as
a translator because it is so simple.

== Constant Operands ==

If the code emitted for a VOP when an argument is constant is very different
than the non-constant case, then it may be desirable to special-case the
operation in VMR conversion by emitting different VOPs.  An example would be if
SVREF is only open-coded when the index is a constant, and turns into a miscop
call otherwise.  We wouldn't want constant references to spuriously allocate
all the miscop linkage registers on the off chance that the offset might not be
constant.  See the :constant feature of VOP primitive type restrictions.

== Supporting Multiple Hardware Configurations ==

A winning way to change emitted code depending on the hardware configuration,
i.e. what FPA is present is to do this using primitive types.  Note that the
Primitive-Type function is VM supplied, and can look at any appropriate
hardware configuration switches.  Short-Float can become 6881-Short-Float,
AFPA-Short-Float, etc.  There would be separate SBs and SCs for the registers
of each kind of FP hardware, with the each hardware-specific primitive type
using the appropriate float register SC.  Then the hardware specific templates
would provide AFPA-Short-Float as the argument type restriction.

Primitive type changes:

The primitive-type structure is given a new %Type slot, which is the CType
structure that is equivalent to this type.  There is also a Guard slot, with,
if true is a function that control whether this primitive type is allowed (due
to hardware configuration, etc.)

We add new :Type and :Guard keywords to Def-Primitive-Type.  Type is the type
specifier that is equivalent (default to the primitive-type name), and Guard is
an expression evaluated in the null environment that controls whether this type
applies (default to none, i.e. constant T).

The Primitive-Type-Type function returns the Lisp CType corresponding to a
primitive type.  This is the %Type unless there is a guard that returns false,
in which case it is the empty type (i.e. NIL).

    But this doesn't do what we want it to do, since we will compute the
    function type for a template at load-time, so they will correspond to whatever
    configuration was in effect then.  Maybe we don't want to dick with guards here
    (if at all).  I guess we can defer this issue until we actually support
    different FP configurations.  But it would seem pretty losing to separately
    flame about all the different FP configurations that could be used to open-code
    + whenever we are forced to closed-code +.

    If we separately report each better possibly applicable template that we
    couldn't use, then it would be reasonable to report any conditional template
    allowed by the configuration.

    But it would probably also be good to give some sort of hint that perhaps it
    would be a good time to make sure you understand how to tell the compiler to
    compile for a particular configuration.  Perhaps if there is a template that
    applies *but for the guard*, then we could give a note.  This way, if someone
    thinks they are being efficient by throwing in lots of declarations, we can let
    them know that they may have to do more.

    I guess the guard should be associated with the template rather than the
    primitive type.  This would allow LTN and friends to easily tell whether a
    template applies in this configuration.  It is also probably more natural for
    some sorts of things: with some hardware variants, it may be that the SBs and
    representations (SCs) are really the same, but there some different allowed
    operations.  In this case, we could easily conditionalize VOPs without the
    increased complexity due to bogus SCs.  If there are different storage
    resources, then we would conditionalize Primitive-Type as well.


== Special-case VMR convert methods ==

    (defun continuation-tn (cont \&optional (check-p t))
      ...)
Return the TN which holds Continuation's first result value.  In general
this may emit code to load the value into a TN.  If Check-P is true, then
when policy indicates, code should be emitted to check that the value satisfies
the continuation asserted type.

    (defun result-tn (cont)
      ...)
Return the TN that Continuation's first value is delivered in.  In general,
may emit code to default any additional values to NIL.

    (defun result-tns (cont n)
      ...)
Similar to Result-TN, except that it returns a list of N result TNs, one
for each of the first N values.


Nearly all open-coded functions should be handled using standard template
selection.  Some (all?) exceptions:
 * List, List* and Vector take arbitrary numbers of arguments.  Could
   implement Vector as a source transform.  Could even do List in a transform
   if we explicitly represent the stack args using %More-Args or something.

 * %Typep varies a lot depending on the type specifier.  We don't want to
   transform it, since we want %Typep as a canonical form so that we can do
   type optimizations.

 * Apply is weird.

 * Funny functions emitted by the compiler: %Listify-Rest-Args, Arg,
   %More-Args, %Special-Bind, %Catch, %Unknown-Values (?), %Unwind-Protect,
   %Unwind, %%Primitive.
]#
