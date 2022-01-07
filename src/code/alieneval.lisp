;;; Any part of the Alien implementation that is not part of the compiler.

(in-package "ALIEN")
(use-package "EXT")
(use-package "SYSTEM")

(export '(alien * array struct union enum function integer signed unsigned
	  boolean values single-float double-float long-float
	  system-area-pointer def-alien-type def-alien-variable sap-alien
	  extern-alien with-alien slot deref addr cast alien-sap alien-size
	  alien-funcall def-alien-routine make-alien free-alien
	  null-alien))

(in-package "ALIEN-INTERNALS")
(in-package "ALIEN")

(import '(alien alien-value alien-value-type parse-alien-type
	  unparse-alien-type alien-type-= alien-subtype-p alien-typep

	  def-alien-type-class def-alien-type-translator def-alien-type-method
	  invoke-alien-type-method

	  alien-type alien-type-p alien-type-bits alien-type-alignment
	  alien-integer-type alien-integer-type-p alien-integer-type-signed
	  alien-boolean-type alien-boolean-type-p
	  alien-enum-type alien-enum-type-p
	  alien-float-type alien-float-type-p
	  alien-single-float-type alien-single-float-type-p
	  alien-double-float-type alien-double-float-type-p
	  alien-long-float-type alien-long-float-type-p
	  alien-pointer-type alien-pointer-type-p alien-pointer-type-to
	  make-alien-pointer-type
	  alien-array-type alien-array-type-p alien-array-type-element-type
	  alien-array-type-dimensions
	  alien-record-type alien-record-type-p alien-record-type-fields
	  alien-record-field alien-record-field-p alien-record-field-name
	  alien-record-field-type alien-record-field-offset
	  alien-function-type alien-function-type-p make-alien-function-type
	  alien-function-type-result-type alien-function-type-arg-types
	  alien-values-type alien-values-type-p alien-values-type-values
	  *values-type-okay*

	  %set-slot %slot-addr %set-deref %deref-addr

	  %heap-alien %set-heap-alien %heap-alien-addr
	  heap-alien-info heap-alien-info-p heap-alien-info-type
	  heap-alien-info-sap-form

	  local-alien %set-local-alien %local-alien-addr
	  local-alien-info local-alien-info-p local-alien-info-type
	  local-alien-info-force-to-memory-p
	  %local-alien-forced-to-memory-p
	  make-local-alien dispose-local-alien note-local-alien-type

	  %cast %sap-alien align-offset

	  extract-alien-value deposit-alien-value naturalize deport
	  compute-lisp-rep-type compute-alien-rep-type
	  compute-extract-lambda compute-deposit-lambda
	  compute-naturalize-lambda compute-deport-lambda)
	"ALIEN-INTERNALS")

(export '(alien alien-value alien-value-type parse-alien-type
	  unparse-alien-type alien-type-= alien-subtype-p alien-typep

	  def-alien-type-class def-alien-type-translator def-alien-type-method
	  invoke-alien-type-method

	  alien-type alien-type-p alien-type-bits alien-type-alignment
	  alien-integer-type alien-integer-type-p alien-integer-type-signed
	  alien-boolean-type alien-boolean-type-p
	  alien-enum-type alien-enum-type-p
	  alien-float-type alien-float-type-p
	  alien-single-float-type alien-single-float-type-p
	  alien-double-float-type alien-double-float-type-p
	  alien-long-float-type alien-long-float-type-p
	  alien-pointer-type alien-pointer-type-p alien-pointer-type-to
	  make-alien-pointer-type
	  alien-array-type alien-array-type-p alien-array-type-element-type
	  alien-array-type-dimensions
	  alien-record-type alien-record-type-p alien-record-type-fields
	  alien-record-field alien-record-field-p alien-record-field-name
	  alien-record-field-type alien-record-field-offset
	  alien-function-type alien-function-type-p make-alien-function-type
	  alien-function-type-result-type alien-function-type-arg-types
	  alien-values-type alien-values-type-p alien-values-type-values
	  *values-type-okay*

	  %set-slot %slot-addr %set-deref %deref-addr

	  %heap-alien %set-heap-alien %heap-alien-addr
	  heap-alien-info heap-alien-info-p heap-alien-info-type
	  heap-alien-info-sap-form

	  local-alien %set-local-alien %local-alien-addr
	  local-alien-info local-alien-info-p local-alien-info-type
	  local-alien-info-force-to-memory-p
	  %local-alien-forced-to-memory-p
	  make-local-alien dispose-local-alien note-local-alien-type

	  %cast %sap-alien align-offset

	  extract-alien-value deposit-alien-value naturalize deport
	  compute-lisp-rep-type compute-alien-rep-type
	  compute-extract-lambda compute-deposit-lambda
	  compute-naturalize-lambda compute-deport-lambda)
	"ALIEN-INTERNALS")


#[ Lisp Equivalents for C Routines

The Unix documentation describes the system interface in terms of C
procedure headers.  The corresponding Lisp functions have an equivalent
interface, tailored for Lisp argument passing conventions and datatypes.

In Lisp, all argument and results are passed by value.  Interface functions
take some fixed number of arguments and return some fixed number of values.
A given "parameter" in the C specification will appear as an argument,
return value, or both, depending on whether it is an In parameter, Out
parameter, or In/Out parameter.  The basic transformation from C routine to
Lisp equivalent is to remove the Out parameters from the call, and treat
them as extra return values.  In/Out parameters appear both as arguments
and return values.  Since Out and In/Out parameters are only conventions in
C, the usage is determined from the documentation.

Thus, the C routine declared as

    kern_return_t lookup(servport, portsname, portsid)
	    port        servport;
	    char        *portsname;
	    int        *portsid;        /* out */
     {
      ...
      *portsid = <expression to compute portsid field>
      return(KERN_SUCCESS);
     }

has as Lisp equivalent something like

    (defun lookup (ServPort PortsName)
      ...
      (values
       success
       <expression to compute portsid field>))

If there are multiple out or in-out arguments, then there are multiple
additional returns values.

Fortunately, programmers rarely have to worry about the nuances of this
translation process, since the names of the arguments and return values are
documented such that `describe' (and the editor "Describe Function Call"
command) list this information.  Since the names of arguments and return
values are usually descriptive, the information that `describe' prints is
usually sufficient to write a call.
]#


#[ Type Translations

Lisp data types have very different representations from those used by
conventional languages such as C.  Since the system interfaces are
designed for conventional languages, Lisp must translate objects to and
from the Lisp representations.  Many simple objects have a direct
translation: integers, characters, strings and floating point numbers
are translated to the corresponding Lisp object.  A number of types,
however, are implemented differently in Lisp for reasons of clarity and
efficiency.

Instances of enumerated types are expressed as keywords in Lisp.  Records,
arrays, and pointer types are implemented with the Alien facility
(described in [Aliens].)  Access functions are defined for these types
which convert fields of records, elements of arrays, or data referenced by
pointers into Lisp objects (possibly another object to be referenced with
another access function).

One should free Alien objects created by constructor functions or returned
from remote procedure calls after last use, in order to free the virtual
memory associated with the objects.  This must be done explicitly since
aliens contain pointers to external data, which is out of the realm of the
garbage collector.  If the memory was obtained from `make-alien' or from a
foreign function call to a routine that used `malloc', then `free-alien'
should be used.  If the alien was created using MACH memory allocation
(e.g. `vm_allocate'), then the storage should be freed using
`vm_deallocate'.
]#


;; FIX merge rest of this page into better places in code

#[ Aliens

[ Introduction to Aliens       ]
[ Alien Types                  ]
[ Alien Operations             ]
[ Alien Variables              ]
[ Alien Data Structure Example ]
[ Loading Unix Object Files    ]
[ Alien Function Calls         ]
[ Step-by-Step Alien Example   ]
]#

#[ Introduction to Aliens

Because of Lisp's emphasis on dynamic memory allocation and garbage
collection, Lisp implementations use unconventional memory representations
for objects.  These representations can create problems when a Lisp program
must share objects with programs written in other languages.  There are
three different approaches to establishing communication:

  * The burden can be placed on the foreign program (and programmer) by
    requiring the use of Lisp object representations.  The main difficulty
    with this approach is that either the foreign program must be written
    with Lisp interaction in mind, or a substantial amount of foreign
    "glue" code must be written to perform the translation.

  * The Lisp system can automatically convert objects back and forth between
    the Lisp and foreign representations.  This is convenient, but
    translation becomes prohibitively slow when large or complex data
    structures must be shared.

  * The Lisp program can directly manipulate foreign objects through the
    use of extensions to the Lisp language.  Most Lisp systems make use of
    this approach, but the language for describing types and expressing
    accesses is often not powerful enough for complex objects to be easily
    manipulated.

Nightshade relies primarily on the automatic conversion and direct
manipulation approaches: Aliens of simple scalar types are automatically
converted, while complex types are directly manipulated in their foreign
representation.  Any foreign objects that can't automatically be converted
into Lisp values are represented by objects of type alien-value.  Since
Lisp is a dynamically typed language, even foreign objects must have a
run-time type; this type information is provided by encapsulating the raw
pointer to the foreign data within an alien-value object.

The Alien type language and operations are most similar to those of the C
language, but Aliens can also be used when communicating with most other
languages that can be linked with C.
]#

#[ Alien Types

Alien types have a description language based on nested list structure.  For
example:

    struct foo {
	int a;
	struct foo *b[100];
    };

has the corresponding Alien type:

    (struct foo
      (a int)
      (b (array (* (struct foo)) 100)))

[ Defining Alien Types       ]
[ Alien Types and Lisp Types ]
[ Alien Type Specifiers      ]
[ The C-Call Package         ]
]#

#[ Defining Alien Types

Types may be either named or anonymous.  With structure and union types, the
name is part of the type specifier, allowing recursively defined types such as:

    (struct foo (a (* (struct foo))))

An anonymous structure or union type is specified by using the name ().
The `with-alien' macro defines a local scope which "captures" any named
type definitions.  Other types are not inherently named, but can be given
named abbreviations using `def-alien-type'.

{function:alien:def-alien-type}
]#

#[ Alien Types and Lisp Types

The Alien types form a subsystem of the Nightshade type system.  An alien
type specifier provides a way to use any Alien type as a Lisp type
specifier.  For example

    (typep foo '(alien (* int)))

can be used to determine whether foo is a pointer to an int.  alien type
specifiers can be used in the same ways as ordinary type specifiers (like
string.)  Alien type declarations are subject to the same precise type
checking as any other declaration (section [precise-type-checks].)

Note that the Alien type system overlaps with normal Lisp type specifiers
in some cases.  For example, the type specifier (alien single-float) is
identical to single-float, since Alien floats are automatically converted
to Lisp floats.  When `type-of' is called on an Alien value that is not
automatically converted to a Lisp value, then it will return an alien type
specifier.
]#

#[ Alien Type Specifiers

Some Alien type names are Lisp symbols, but the names are still exported
from the alien package, so it is legal to say alien:single-float.  These
are the basic Alien type specifiers:

type:type:Alien type}
A pointer to an object of the specified TYPE.  If TYPE is t, then it means
a pointer to anything, similar to "void *" in ANSI C.  Currently, the only
way to detect a null pointer is:

    (zerop (sap-int (alien-sap \var{ptr})))

refer to [System Area Pointers]

{type:alien::alien-array-type}
{type:alien::alien-integer-type}
{type:alien::alien-boolean-type}

{type:alien::alien-single-float-type}
{type:alien::alien-double-float-type}
{type:alien::alien-function-type}
{type:alien::alien-system-area-pointer-type}
]#

#[ Alien Operations

This section describes the basic operations on Alien values.

[ Alien Access Operations   ]
[ Alien Coercion Operations ]
[ Alien Dynamic Allocation  ]
]#

#[ Alien Access Operations

{function:alien:deref}
{function:alien:slot}
]#

#[ Alien Coercion Operations

{function:alien:addr}
{function:alien:cast}
{function:alien:sap-alien}
{function:alien:alien-sap}
]#

#[ Alien Dynamic Allocation

Dynamic Aliens are allocated using the `malloc' library, so foreign code
can call `free' on the result of `make-alien', and Lisp code can call
`free-alien' on objects allocated by foreign code.

{function:alien:make-alien}
{function:alien:free-alien}

The function `with-alien' stack-allocates Aliens (described in [Local Alien
Variables]).
]#

#[ Alien Function Calls

The foreign function call interface allows a Lisp program to call functions
written in other languages.  The current implementation of the foreign
function call interface assumes a C calling convention and thus routines
written in any language that adheres to this convention may be called from
Lisp.

Lisp sets up various interrupt handling routines and other environment
information when it first starts up, and expects these to be in place at all
times.  The C functions called by Lisp should either not change the
environment, especially the interrupt entry points, or should make sure
that these entry points are restored when the C function returns to Lisp.
If a C function makes changes without restoring things to the way they were
when the C function was entered, there is no telling what will happen.


[ alien-funcall             ]           The alien-funcall Primitive
[ def-alien-routine         ]           The def-alien-routine Macro
[ def-alien-routine Example ]
[ Calling Lisp from C       ]
]#

#[ alien-funcall

{function:alien:alien-funcall}

Here is an example which allocates a (struct foo), calls a foreign
function to initialize it, then returns a Lisp vector of all the
(* (struct foo)) objects filled in by the foreign call:

    ;; Allocate a foo on the stack.
    (with-alien ((f (struct foo)))
      ;;
      ;; Call some C function to fill in foo fields.
      (alien-funcall (extern-alien "mangle_foo" (function void (* foo)))
		     (addr f))
      ;;
      ;; Find how many foos to use by getting the A field.
      (let* ((num (slot f 'a))
	     (result (make-array num)))
	;;
	;; Get a pointer to the array so that we don't have to keep extracting it:
	(with-alien ((a (* (array (* (struct foo)) 100)) (addr (slot f 'b))))
	  ;;
	  ;; Loop over the first N elements and stash them in the result vector.
	  (dotimes (i num)
	    (setf (svref result i) (deref (deref a) i)))
	  result)))
]#

#[ def-alien-routine

{function:alien:def-alien-routine}
]#

#[ def-alien-routine Example

Consider the C function `cfoo' with the following calling convention:

    cfoo (str, a, i)
	char *str;
	char *a; /* update */
	int *i; /* out */
    {
    /* Body of cfoo. */
    }

which can be described by the following call to `def-alien-routine':

    (def-alien-routine "cfoo" void
      (str c-string)
      (a char :in-out)
      (i int :out))

The Lisp function `cfoo' will have two arguments (STR and A) and two return
values (A and I).
]#

;; FIX file refs
#[ Calling Lisp from C

Calling Lisp functions from C is sometimes possible, but is rather hackish.
See funcall0 ... funcall3 in the file "lisp/arch.h".  The arguments must be
valid Lisp object descriptors (e.g.  fixnums must be left-shifted by 2.)
See "compiler/generic/objdef.lisp" or the derived file "lisp/internals.h"
for details of the object representation.  "lisp/internals.h" is
mechanically generated, and is not part of the source distribution.  It is
distributed in the "docs/" directory of the binary distribution.

Note that the garbage collector moves objects, and won't be able to fix up
any references in C variables, so either turn GC off or don't keep Lisp
pointers in C data unless they are to statically allocated objects.  You
can use `purify' to place live data structures in static space so that they
won't move during GC.
]#


;;;; Utility functions.

(defun align-offset (offset alignment)
  (let ((extra (rem offset alignment)))
    (if (zerop extra) offset (+ offset (- alignment extra)))))

(defun guess-alignment (bits)
  (cond ((null bits) nil)
	#-x86 ((> bits 32) 64)
	((> bits 16) 32)
	((> bits 8) 16)
	((> bits 1) 8)
	(t 1)))


;;;; Alien-type-info stuff.

(eval-when (compile eval load)

(defstruct (alien-type-class
	    (:print-function %print-alien-type-class))
  (name nil :type symbol)
  (include nil :type (or null alien-type-class))
  (unparse nil :type (or null function))
  (type= nil :type (or null function))
  (lisp-rep nil :type (or null function))
  (alien-rep nil :type (or null function))
  (extract-gen nil :type (or null function))
  (deposit-gen nil :type (or null function))
  (naturalize-gen nil :type (or null function))
  (deport-gen nil :type (or null function))
  ;; Cast?
  (arg-tn nil :type (or null function))
  (result-tn nil :type (or null function))
  (subtypep nil :type (or null function)))

(defun %print-alien-type-class (type-class stream depth)
  (declare (ignore depth))
  (print-unreadable-object (type-class stream :type t)
    (prin1 (alien-type-class-name type-class) stream)))

(defvar *alien-type-classes* (make-hash-table :test #'eq))

(defun alien-type-class-or-lose (name)
  (or (gethash name *alien-type-classes*)
      (error "No alien type class ~S" name)))

(defun create-alien-type-class-if-necessary (name include)
  (let ((old (gethash name *alien-type-classes*))
	(include (and include (alien-type-class-or-lose include))))
    (if old
	(setf (alien-type-class-include old) include)
	(setf (gethash name *alien-type-classes*)
	      (make-alien-type-class :name name :include include)))))

(defconstant method-slot-alist
  '((:unparse . alien-type-class-unparse)
    (:type= . alien-type-class-type=)
    (:subtypep . alien-type-class-subtypep)
    (:lisp-rep . alien-type-class-lisp-rep)
    (:alien-rep . alien-type-class-alien-rep)
    (:extract-gen . alien-type-class-extract-gen)
    (:deposit-gen . alien-type-class-deposit-gen)
    (:naturalize-gen . alien-type-class-naturalize-gen)
    (:deport-gen . alien-type-class-deport-gen)
    ;; Cast?
    (:arg-tn . alien-type-class-arg-tn)
    (:result-tn . alien-type-class-result-tn)))

(defun method-slot (method)
  (cdr (or (assoc method method-slot-alist)
	   (error "No method ~S" method))))

); eval-when

;;; Define a keyword "BOA" constructor in order to reference the slots
;;; names in init forms.
;;;
(defmacro def-alien-type-class ((name &key include include-args) &rest slots)
  "Globally define NAME as a shorthand for the Alien type TYPE.  When
   introducing global structure and union type definitions, NAME may be (),
   in which case the name to define is taken from the type's name."
  (let ((defstruct-name
	 (intern (concatenate 'string "ALIEN-" (symbol-name name) "-TYPE"))))
    (multiple-value-bind
	(include include-defstruct overrides)
	(etypecase include
	  (null
	   (values nil 'alien-type nil))
	  (symbol
	   (values
	    include
	    (intern (concatenate 'string
				 "ALIEN-" (symbol-name include) "-TYPE"))
	    nil))
	  (list
	   (values
	    (car include)
	    (intern (concatenate 'string
				 "ALIEN-" (symbol-name (car include)) "-TYPE"))
	    (cdr include))))
      `(progn
	 (eval-when (compile load eval)
	   (create-alien-type-class-if-necessary ',name ',(or include 'root)))
	 (defstruct (,defstruct-name
			(:include ,include-defstruct
				  (:class ',name)
				  ,@overrides)
			(:constructor
			 ,(intern (concatenate 'string "MAKE-"
					       (string defstruct-name)))
			 (&key class bits alignment
			       ,@(mapcar #'(lambda (x)
					     (if (atom x) x (car x)))
					 (if (stringp (car slots))
					     (cdr slots)
					     slots))
			       ,@include-args)))
	   ,@slots)))))

(defmacro def-alien-type-method ((class method) lambda-list &rest body)
  (let ((defun-name (intern (concatenate 'string
					 (symbol-name class)
					 "-"
					 (symbol-name method)
					 "-METHOD"))))
    `(progn
       (defun ,defun-name ,lambda-list
	 ,@body)
       (setf (,(method-slot method) (alien-type-class-or-lose ',class))
	     #',defun-name))))

(defmacro invoke-alien-type-method (method type &rest args)
  (let ((slot (method-slot method)))
    (once-only ((type type))
      `(funcall (do ((class (alien-type-class-or-lose (alien-type-class ,type))
			    (alien-type-class-include class)))
		    ((null class)
		     (error "Method ~S not defined for ~S"
			    ',method (alien-type-class ,type)))
		  (let ((fn (,slot class)))
		    (when fn
		      (return fn))))
		,type ,@args))))


;;;; Alien-type defstruct.

(eval-when (compile load eval)
  (create-alien-type-class-if-necessary 'root nil))

(defstruct (alien-type
	    (:print-function %print-alien-type)
	    (:make-load-form-fun :just-dump-it-normally)
	    (:constructor make-alien-type (&key class bits alignment)))
  (class 'root :type symbol)
  (bits nil :type (or null unsigned-byte))
  (alignment (guess-alignment bits) :type (or null unsigned-byte)))

(defun %print-alien-type (type stream depth)
  (declare (ignore depth))
  (print-unreadable-object (type stream :type t)
    (prin1 (unparse-alien-type type) stream)))


;;;; Type parsing and unparsing.

(defvar *auxiliary-type-definitions* nil)
(defvar *new-auxiliary-types*)

;;; WITH-AUXILIARY-ALIEN-TYPES -- internal.
;;;
;;; Process stuff in a new scope.
;;;
(defmacro with-auxiliary-alien-types (&body body)
  `(let ((*auxiliary-type-definitions*
	  (if (boundp '*new-auxiliary-types*)
	      (append *new-auxiliary-types* *auxiliary-type-definitions*)
	      *auxiliary-type-definitions*))
	 (*new-auxiliary-types* nil))
     ,@body))

;;; PARSE-ALIEN-TYPE -- public
;;;
(defun parse-alien-type (type)
  "Parse the list structure $type as an alien type specifier and return the
   resultant alien-type structure."
  (if (boundp '*new-auxiliary-types*)
      (%parse-alien-type type)
      (let ((*new-auxiliary-types* nil))
	(%parse-alien-type type))))

(defun %parse-alien-type (type)
  (if (consp type)
      (let ((translator (info alien-type translator (car type))))
	(unless translator
	  (error "Unknown alien type: ~S" type))
	(funcall translator type))
      (case (info alien-type kind type)
	(:primitive
	 (let ((translator (info alien-type translator type)))
	   (unless translator
	     (error "No translator for primitive alien type ~S?" type))
	   (funcall translator (list type))))
	(:defined
	 (or (info alien-type definition type)
	     (error "Definition missing for alien type ~S?" type)))
	(:unknown
	 (error "Unknown alien type: ~S" type)))))

(defun auxiliary-alien-type (kind name)
  (flet ((aux-defn-matches (x)
	   (and (eq (first x) kind) (eq (second x) name))))
    (let ((in-auxiliaries
	   (or (find-if #'aux-defn-matches *new-auxiliary-types*)
	       (find-if #'aux-defn-matches *auxiliary-type-definitions*))))
      (if in-auxiliaries
	  (values (third in-auxiliaries) t)
	  (ecase kind
	    (:struct
	     (info alien-type struct name))
	    (:union
	     (info alien-type union name))
	    (:enum
	     (info alien-type enum name)))))))

(defun %set-auxiliary-alien-type (kind name defn)
  (flet ((aux-defn-matches (x)
	   (and (eq (first x) kind) (eq (second x) name))))
    (when (find-if #'aux-defn-matches *new-auxiliary-types*)
      (error "Attempt to multiple define ~A ~S." kind name))
    (when (find-if #'aux-defn-matches *auxiliary-type-definitions*)
      (error "Attempt to shadow definition of ~A ~S." kind name)))
  (push (list kind name defn) *new-auxiliary-types*)
  defn)

(defsetf auxiliary-alien-type %set-auxiliary-alien-type)

(defun verify-local-auxiliaries-okay ()
  (dolist (info *new-auxiliary-types*)
    (destructuring-bind (kind name defn) info
      (declare (ignore defn))
      (when (ecase kind
	      (:struct
	       (info alien-type struct name))
	      (:union
	       (info alien-type union name))
	      (:enum
	       (info alien-type enum name)))
	(error "Attempt to shadow definition of ~A ~S." kind name)))))

;;; *record-type-already-unparsed* -- internal
;;;
;;; Holds the list of record types that have already been unparsed.  This is
;;; used to keep from outputing the slots again if the same structure shows
;;; up twice.
;;;
(defvar *record-types-already-unparsed*)

;;; UNPARSE-ALIEN-TYPE -- public.
;;;
(defun unparse-alien-type (type)
  "Convert the alien-type structure $type back into a list specification of
   the type."
  (declare (type alien-type type))
  (let ((*record-types-already-unparsed* nil))
    (%unparse-alien-type type)))

;;; %UNPARSE-ALIEN-TYPE -- internal.
;;;
;;; Does all the work of UNPARSE-ALIEN-TYPE.  It's seperate because we need
;;; to recurse inside the binding of *record-types-already-unparsed*.
;;;
(defun %unparse-alien-type (type)
  (invoke-alien-type-method :unparse type))


;;;; Alien type defining stuff.

(defmacro def-alien-type-translator (name lambda-list &body body)
  (let ((whole (gensym))
	(defun-name (intern (concatenate 'string
					 "ALIEN-"
					 (symbol-name name)
					 "-TYPE-TRANSLATOR"))))
    (multiple-value-bind
	(body decls docs)
	(lisp::parse-defmacro lambda-list whole body name
			      'def-alien-type-translator)
      `(progn
	 (defun ,defun-name (,whole)
	   ,decls
	   (block ,name
	     ,body))
	 (%def-alien-type-translator ',name #',defun-name ,docs)))))

(defun %def-alien-type-translator (name translator docs)
  (setf (info alien-type kind name) :primitive)
  (setf (info alien-type translator name) translator)
  (clear-info alien-type definition name)
  (setf (documentation name 'alien-type) docs)
  name)

(defmacro def-alien-type (name type)
  "Define the alien type $name to be equivalent to $type.  $name may be ()
   for struct and union types, in which case the name is taken from the
   type specifier."
  (with-auxiliary-alien-types
    (let ((alien-type (parse-alien-type type)))
      `(eval-when (compile load eval)
	 ,@(when *new-auxiliary-types*
	     `((%def-auxiliary-alien-types ',*new-auxiliary-types*)))
	 ,@(when name
	     `((%def-alien-type ',name ',alien-type)))))))

(defun %def-auxiliary-alien-types (types)
  (dolist (info types)
    (destructuring-bind (kind name defn) info
      (macrolet ((frob (kind)
		   `(let ((old (info alien-type ,kind name)))
		      (unless (or (null old) (alien-type-= old defn))
			(warn "Redefining ~A ~S to be:~%  ~S,~%was:~%  ~S"
			      kind name defn old))
		      (setf (info alien-type ,kind name) defn))))
	(ecase kind
	  (:struct (frob struct))
	  (:union (frob union))
	  (:enum (frob enum)))))))

(defun %def-alien-type (name new)
  (ecase (info alien-type kind name)
    (:primitive
     (error "~S is a built-in alien type." name))
    (:defined
     (let ((old (info alien-type definition name)))
       (unless (or (null old) (alien-type-= new old))
	 (warn "Redefining ~S to be:~%  ~S,~%was~%  ~S" name
	       (unparse-alien-type new) (unparse-alien-type old)))))
    (:unknown))
  (setf (info alien-type definition name) new)
  (setf (info alien-type kind name) :defined)
  name)


;;;; Interfaces to the different methods

(defun alien-type-= (type1 type2)
  "Return t iff $type1 and $type2 describe equivalent alien types."
  (or (eq type1 type2)
      (and (eq (alien-type-class type1)
	       (alien-type-class type2))
	   (invoke-alien-type-method :type= type1 type2))))

(defun alien-subtype-p (type1 type2)
  "Return t iff the alien type $type1 is a subtype of $type2.  Currently,
   the only supported subtype relationships are is that any pointer type is
   a subtype of (* t), and any array type first dimension will match (array
   <eltype> nil ...).  Otherwise, the two types have to be ALIEN-TYPE-=."
  (or (eq type1 type2)
      (invoke-alien-type-method :subtypep type1 type2)))

(defun alien-typep (object type)
  "Return t iff $object is an alien of type $type."
  (let ((lisp-rep-type (compute-lisp-rep-type type)))
    (if lisp-rep-type
	(typep object lisp-rep-type)
	(and (alien-value-p object)
	     (alien-subtype-p (alien-value-type object) type)))))

(defun compute-naturalize-lambda (type)
  `(lambda (alien ignore)
     (declare (ignore ignore))
     ,(invoke-alien-type-method :naturalize-gen type 'alien)))

(defun compute-deport-lambda (type)
  (declare (type alien-type type))
  (multiple-value-bind
      (form value-type)
      (invoke-alien-type-method :deport-gen type 'value)
    `(lambda (value ignore)
       (declare (type ,(or value-type
			   (compute-lisp-rep-type type)
			   `(alien ,type))
		      value)
		(ignore ignore))
       ,form)))

(defun compute-extract-lambda (type)
  `(lambda (sap offset ignore)
     (declare (type system-area-pointer sap)
	      (type unsigned-byte offset)
	      (ignore ignore))
     (naturalize ,(invoke-alien-type-method :extract-gen type 'sap 'offset)
		 ',type)))

(defun compute-deposit-lambda (type)
  (declare (type alien-type type))
  `(lambda (sap offset ignore value)
     (declare (type system-area-pointer sap)
	      (type unsigned-byte offset)
	      (ignore ignore))
     (let ((value (deport value ',type)))
       ,(invoke-alien-type-method :deposit-gen type 'sap 'offset 'value)
       ;; Note: the reason we don't just return the pre-deported value
       ;; is because that would inhibit any (deport (naturalize ...))
       ;; optimizations that might have otherwise happen.  Re-naturalizing
       ;; the value might cause extra consing, but is flushable, so probably
       ;; results in better code.
       (naturalize value ',type))))

(defun compute-lisp-rep-type (type)
  (invoke-alien-type-method :lisp-rep type))

(defun compute-alien-rep-type (type)
  (invoke-alien-type-method :alien-rep type))


;;;; Default methods.

(def-alien-type-method (root :unparse) (type)
  `(!!unknown-alien-type!! ,(type-of type)))

(def-alien-type-method (root :type=) (type1 type2)
  (declare (ignore type1 type2))
  t)

(def-alien-type-method (root :subtypep) (type1 type2)
  (alien-type-= type1 type2))

(def-alien-type-method (root :lisp-rep) (type)
  (declare (ignore type))
  nil)

(def-alien-type-method (root :alien-rep) (type)
  (declare (ignore type))
  '*)

(def-alien-type-method (root :naturalize-gen) (type alien)
  (declare (ignore alien))
  (error "Cannot represent ~S typed aliens." type))

(def-alien-type-method (root :deport-gen) (type object)
  (declare (ignore object))
  (error "Cannot represent ~S typed aliens." type))

(def-alien-type-method (root :extract-gen) (type sap offset)
  (declare (ignore sap offset))
  (error "Cannot represent ~S typed aliens." type))

(def-alien-type-method (root :deposit-gen) (type sap offset value)
  `(setf ,(invoke-alien-type-method :extract-gen type sap offset) ,value))

(def-alien-type-method (root :arg-tn) (type state)
  (declare (ignore state))
  (error "Cannot pass aliens of type ~S as arguments to call-out"
	 (unparse-alien-type type)))

(def-alien-type-method (root :result-tn) (type state)
  (declare (ignore state))
  (error "Cannot return aliens of type ~S from call-out"
	 (unparse-alien-type type)))


;;;; The INTEGER type.

(def-alien-type-class (integer)
  "Alien \"integer\", \"signed\" and \"unsigned\" types.  Types integer and
   signed are the same: a signed integer with the specified number of bits
   precision.  The upper limit on integer precision is determined by the
   machine's word size.  If no size is specified, the maximum size will be
   used.  Type unsigned is like signed and integer, only unsigned."
  (signed t :type (member t nil)))

(def-alien-type-translator signed (&optional (bits vm:word-bits))
  (make-alien-integer-type :bits bits))

(def-alien-type-translator integer (&optional (bits vm:word-bits))
  (make-alien-integer-type :bits bits))

(def-alien-type-translator unsigned (&optional (bits vm:word-bits))
  (make-alien-integer-type :bits bits :signed nil))

(def-alien-type-method (integer :unparse) (type)
  (list (if (alien-integer-type-signed type) 'signed 'unsigned)
	(alien-integer-type-bits type)))

(def-alien-type-method (integer :type=) (type1 type2)
  (and (eq (alien-integer-type-signed type1)
	   (alien-integer-type-signed type2))
       (= (alien-integer-type-bits type1)
	  (alien-integer-type-bits type2))))

(def-alien-type-method (integer :lisp-rep) (type)
  (list (if (alien-integer-type-signed type) 'signed-byte 'unsigned-byte)
	(alien-integer-type-bits type)))

(def-alien-type-method (integer :alien-rep) (type)
  (list (if (alien-integer-type-signed type) 'signed-byte 'unsigned-byte)
	(alien-integer-type-bits type)))

(def-alien-type-method (integer :naturalize-gen) (type alien)
  (declare (ignore type))
  alien)

(def-alien-type-method (integer :deport-gen) (type value)
  (declare (ignore type))
  value)

(def-alien-type-method (integer :extract-gen) (type sap offset)
  (declare (type alien-integer-type type))
  (let ((ref-fun
	 (if (alien-integer-type-signed type)
	  (case (alien-integer-type-bits type)
	    (8 'signed-sap-ref-8)
	    (16 'signed-sap-ref-16)
	    (32 'signed-sap-ref-32)
	    (64 'signed-sap-ref-64))
	  (case (alien-integer-type-bits type)
	    (8 'sap-ref-8)
	    (16 'sap-ref-16)
	    (32 'sap-ref-32)
	    (64 'sap-ref-64)))))
    (if ref-fun
	`(,ref-fun ,sap (/ ,offset vm:byte-bits))
	(error "Cannot extract ~D bit integers."
	       (alien-integer-type-bits type)))))


;;;; The BOOLEAN type.

(def-alien-type-class (boolean :include integer :include-args (signed))
  "Similar to an enumeration type that maps 0 to () and all other values to
   t.  BITS determines the amount of storage allocated to hold the truth
   value.")

(def-alien-type-translator boolean (&optional (bits vm:word-bits))
  (make-alien-boolean-type :bits bits :signed nil))

(def-alien-type-method (boolean :unparse) (type)
  `(boolean ,(alien-boolean-type-bits type)))

(def-alien-type-method (boolean :lisp-rep) (type)
  (declare (ignore type))
  `(member t nil))

(def-alien-type-method (boolean :naturalize-gen) (type alien)
  (declare (ignore type))
  `(not (zerop ,alien)))

(def-alien-type-method (boolean :deport-gen) (type value)
  (declare (ignore type))
  `(if ,value 1 0))


;;;; The ENUM type.

(def-alien-type-class (enum :include (integer (:bits 32))
			    :include-args (signed))
  name		; name of this enum (if any)
  from		; alist from keywords to integers.
  to		; alist or vector from integers to keywords.
  kind		; Kind of from mapping, :vector or :alist.
  offset)	; Offset to add to value for :vector from mapping.

(def-alien-type-translator enum (&whole type name &rest mappings)
  (cond (mappings
	 (let ((result (parse-enum name mappings)))
	   (when name
	     (multiple-value-bind
		 (old old-p)
		 (auxiliary-alien-type :enum name)
	       (when old-p
		 (unless (alien-type-= result old)
		   (warn "Redefining alien enum ~S" name))))
	     (setf (auxiliary-alien-type :enum name) result))
	   result))
	(name
	 (multiple-value-bind
	     (result found)
	     (auxiliary-alien-type :enum name)
	   (unless found
	     (error "Unknown enum type: ~S" name))
	   result))
	(t
	 (error "Empty enum type: ~S" type))))

(defun parse-enum (name elements)
  (if (null elements)
      (error "An anumeration must contain at least one element."))
  (let ((min nil)
	(max nil)
	(from-alist ())
	(prev -1))
    (declare (list from-alist))
    (dolist (el elements)
      (multiple-value-bind
	  (sym val)
	  (if (listp el)
	      (values (first el) (second el))
	      (values el (1+ prev)))
	(setf prev val)
	(unless (keywordp sym)
	  (error "Enumeration element ~S is not a keyword." sym))
	(unless (integerp val)
	  (error "Element value ~S is not an integer." val))
	(unless (and max (> max val)) (setq max val))
	(unless (and min (< min val)) (setq min val))
	(when (rassoc val from-alist)
	  (error "Element value ~S used more than once." val))
	(when (assoc sym from-alist :test #'eq)
	  (error "Enumeration element ~S used more than once." sym))
	(push (cons sym val) from-alist)))
    (let* ((signed (minusp min))
	   (min-bits (if signed
			 (1+ (max (integer-length min)
				  (integer-length max)))
			 (integer-length max))))
      (when (> min-bits 32)
	(error "Can't represent enums needing more than 32 bits."))
      (setf from-alist (sort from-alist #'< :key #'cdr))
      (cond
       ;;
       ;; If range is at least 20% dense, use vector mapping.  Crossover
       ;; point solely on basis of space would be 25%.  Vector mapping
       ;; is always faster, so give the benefit of the doubt.
       ((< 0.2 (/ (float (length from-alist)) (float (- max min))))
	;;
	;; If offset is small and ignorable, ignore it to save time.
	(when (< 0 min 10) (setq min 0))
	(let ((to (make-array (1+ (- max min)))))
	  (dolist (el from-alist)
	    (setf (svref to (- (cdr el) min)) (car el)))
	  (make-alien-enum-type :name name :signed signed
				:from from-alist :to to :kind
				:vector :offset (- min))))
       (t
	(make-alien-enum-type :name name :signed signed
			      :from from-alist
			      :to (mapcar #'(lambda (x) (cons (cdr x) (car x)))
					  from-alist)
			      :kind :alist))))))

(def-alien-type-method (enum :unparse) (type)
  `(enum ,(alien-enum-type-name type)
	 ,@(let ((prev -1))
	     (mapcar #'(lambda (mapping)
			 (let ((sym (car mapping))
			       (value (cdr mapping)))
			   (prog1
			       (if (= (1+ prev) value)
				   sym
				   `(,sym ,value))
			     (setf prev value))))
		     (alien-enum-type-from type)))))

(def-alien-type-method (enum :type=) (type1 type2)
  (and (eq (alien-enum-type-name type1)
	   (alien-enum-type-name type2))
       (equal (alien-enum-type-from type1)
	      (alien-enum-type-from type2))))

(def-alien-type-method (enum :lisp-rep) (type)
  `(member ,@(mapcar #'car (alien-enum-type-from type))))

(def-alien-type-method (enum :naturalize-gen) (type alien)
  (ecase (alien-enum-type-kind type)
    (:vector
     `(svref ',(alien-enum-type-to type)
	     (+ ,alien ,(alien-enum-type-offset type))))
    (:alist
     `(ecase ,alien
	,@(mapcar #'(lambda (mapping)
		      `(,(car mapping) ,(cdr mapping)))
		  (alien-enum-type-to type))))))

(def-alien-type-method (enum :deport-gen) (type value)
  `(ecase ,value
     ,@(mapcar #'(lambda (mapping)
		   `(,(car mapping) ,(cdr mapping)))
	       (alien-enum-type-from type))))


;;;; The FLOAT types.

(def-alien-type-class (float)
  (type (required-argument) :type symbol))

(def-alien-type-method (float :unparse) (type)
  (alien-float-type-type type))

(def-alien-type-method (float :lisp-rep) (type)
  (alien-float-type-type type))

(def-alien-type-method (float :alien-rep) (type)
  (alien-float-type-type type))

(def-alien-type-method (float :naturalize-gen) (type alien)
  (declare (ignore type))
  alien)

(def-alien-type-method (float :deport-gen) (type value)
  (declare (ignore type))
  value)

(def-alien-type-class (single-float :include (float (:bits 32))
				    :include-args (type))
  "A floating-point number in IEEE single format.")

(def-alien-type-translator single-float ()
  (make-alien-single-float-type :type 'single-float))

(def-alien-type-method (single-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-single ,sap (/ ,offset vm:byte-bits)))


(def-alien-type-class (double-float :include (float (:bits 64))
				    :include-args (type))
  "A floating-point number in IEEE double format.")

(def-alien-type-translator double-float ()
  (make-alien-double-float-type :type 'double-float))

(def-alien-type-method (double-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-double ,sap (/ ,offset vm:byte-bits)))

#+long-float
(def-alien-type-class (long-float :include (float (:bits #+x86 96 #+sparc 128))
				  :include-args (type)))

#+long-float
(def-alien-type-translator long-float ()
  (make-alien-long-float-type :type 'long-float))

#+long-float
(def-alien-type-method (long-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-long ,sap (/ ,offset vm:byte-bits)))


;;;; The SAP type.

(def-alien-type-class (system-area-pointer)
  "A pointer which is represented in Lisp as a system-area-pointer object
   ([System Area Pointers].)")

(def-alien-type-translator system-area-pointer ()
  (make-alien-system-area-pointer-type :bits #-alpha vm:word-bits #+alpha 64))

(def-alien-type-method (system-area-pointer :unparse) (type)
  (declare (ignore type))
  'system-area-pointer)

(def-alien-type-method (system-area-pointer :lisp-rep) (type)
  (declare (ignore type))
  'system-area-pointer)

(def-alien-type-method (system-area-pointer :alien-rep) (type)
  (declare (ignore type))
  'system-area-pointer)

(def-alien-type-method (system-area-pointer :naturalize-gen) (type alien)
  (declare (ignore type))
  alien)

(def-alien-type-method (system-area-pointer :deport-gen) (type object)
  (declare (ignore type))
  object)

(def-alien-type-method (system-area-pointer :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-sap ,sap (/ ,offset vm:byte-bits)))


;;;; The ALIEN-VALUE type.

(def-alien-type-class (alien-value :include system-area-pointer))

(def-alien-type-method (alien-value :lisp-rep) (type)
  (declare (ignore type))
  nil)

(def-alien-type-method (alien-value :naturalize-gen) (type alien)
  `(%sap-alien ,alien ',type))

(def-alien-type-method (alien-value :deport-gen) (type value)
  (declare (ignore type))
  `(alien-sap ,value))


;;;; The POINTER type.

(def-alien-type-class (pointer :include (alien-value (:bits
						      #-alpha vm:word-bits
						      #+alpha 64)))
  (to nil :type (or alien-type null)))

(def-alien-type-translator * (to)
  (make-alien-pointer-type :to (if (eq to t) nil (parse-alien-type to))))

(def-alien-type-method (pointer :unparse) (type)
  (let ((to (alien-pointer-type-to type)))
    `(* ,(if to
	     (%unparse-alien-type to)
	     t))))

(def-alien-type-method (pointer :type=) (type1 type2)
  (let ((to1 (alien-pointer-type-to type1))
	(to2 (alien-pointer-type-to type2)))
    (if to1
	(if to2
	    (alien-type-= to1 to2)
	    nil)
	(null to2))))

(def-alien-type-method (pointer :subtypep) (type1 type2)
  (and (alien-pointer-type-p type2)
       (let ((to1 (alien-pointer-type-to type1))
	     (to2 (alien-pointer-type-to type2)))
	 (if to1
	     (if to2
		 (alien-subtype-p to1 to2)
		 t)
	     (null to2)))))

(def-alien-type-method (pointer :deport-gen) (type value)
  (values
   `(etypecase ,value
      (null
       (int-sap 0))
      (system-area-pointer
       ,value)
      ((alien ,type)
       (alien-sap ,value)))
   `(or null system-area-pointer (alien ,type))))


;;;; The MEM-BLOCK type.

(def-alien-type-class (mem-block :include alien-value))

(def-alien-type-method (mem-block :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap+ ,sap (/ ,offset vm:byte-bits)))

(def-alien-type-method (mem-block :deposit-gen) (type sap offset value)
  (let ((bits (alien-mem-block-type-bits type)))
    (unless bits
      (error "Cannot deposit aliens of type ~S (unknown size)." type))
    `(kernel:system-area-copy ,value 0 ,sap ,offset ',bits)))


;;;; The ARRAY type.

(def-alien-type-class (array :include mem-block)
  "Alien type \"array\".  An array of DIMENSIONS, holding elements of type
   TYPE.  Note that (* int) and (array int) are considered to be separate
   types FIX types are checked; pointer and array types must be explicitly
   coerced using `cast'.  Arrays are accessed using `deref', passing the
   indices as additional arguments.  Elements are stored in column-major
   order (as in C), so the first dimension determines only the size of the
   memory block, and not the layout of the higher dimensions.  An array
   whose first dimension is variable may be specified by using () as the
   first dimension.  Fixed-size arrays can be allocated as array elements,
   structure slots or `with-alien' variables.  Dynamic arrays can only be
   allocated using `make-alien'.

   Also alien type \"struct\".  A structure type with the specified
   NAME and FIELDS.  Fields are allocated at the same positions used by the
   implementation's C compiler.  BITS is intended for C-like bit field
   support, but is currently unused.  If NAME is (), then the type is
   anonymous.

   If a named Alien struct specifier is passed to `def-alien-type' or
   `with-alien', then this defines, respectively, a new global or local
   Alien structure type.  If no FIELDS are specified, then the fields are
   taken from the current (local or global) Alien structure type definition
   of NAME.

   Also alien type \"union\".  Similar to struct, but defines a union type.
   All fields are allocated at the same offset, and the size of the union
   is the size of the largest field.  The programmer must determine which
   field is active from context.

   Also Alien type \"enum\". An enumeration type that maps between integer
   values and keywords.  If NAME is (), then the type is anonymous.  Each
   SPEC is either a keyword, or a list (KEYWORD VALUE).  If INTEGER is (),
   then it defaults to one greater than the value for the preceding spec
   (or to zero if it is the first spec.)"
  (element-type (required-argument) :type alien-type)
  (dimensions (required-argument) :type list))

(def-alien-type-translator array (ele-type &rest dims)
  (when dims
    (unless (typep (first dims) '(or kernel:index null))
      (error "First dimension is not a non-negative fixnum or NIL: ~S"
	     (first dims)))
    (let ((loser (find-if-not #'(lambda (x) (typep x 'kernel:index))
			      (rest dims))))
      (when loser
	(error "Dimension is not a non-negative fixnum: ~S" loser))))

  (let ((type (parse-alien-type ele-type)))
    (make-alien-array-type
     :element-type type
     :dimensions dims
     :alignment (alien-type-alignment type)
     :bits (if (and (alien-type-bits type)
		    (every #'integerp dims))
	       (* (align-offset (alien-type-bits type)
				(alien-type-alignment type))
		  (reduce #'* dims))))))

(def-alien-type-method (array :unparse) (type)
  `(array ,(%unparse-alien-type (alien-array-type-element-type type))
	  ,@(alien-array-type-dimensions type)))

(def-alien-type-method (array :type=) (type1 type2)
  (and (equal (alien-array-type-dimensions type1)
	      (alien-array-type-dimensions type2))
       (alien-type-= (alien-array-type-element-type type1)
		     (alien-array-type-element-type type2))))

(def-alien-type-method (array :subtypep) (type1 type2)
  (and (alien-array-type-p type2)
       (let ((dim1 (alien-array-type-dimensions type1))
	     (dim2 (alien-array-type-dimensions type2)))
	 (and (= (length dim1) (length dim2))
	      (or (and dim2
		       (null (car dim2))
		       (equal (cdr dim1) (cdr dim2)))
		  (equal dim1 dim2))
	      (alien-subtype-p (alien-array-type-element-type type1)
			       (alien-array-type-element-type type2))))))


;;;; The RECORD type.

(defstruct (alien-record-field
	    (:print-function %print-alien-field)
	    (:make-load-form-fun :just-dump-it-normally))
  (name (required-argument) :type symbol)
  (type (required-argument) :type alien-type)
  (bits nil :type (or unsigned-byte null))
  (offset 0 :type unsigned-byte))

(defun %print-alien-field (field stream depth)
  (declare (ignore depth))
  (print-unreadable-object (field stream :type t)
    (funcall (formatter "~S ~S~@[:~D~]")
	     stream
	     (alien-record-field-type field)
	     (alien-record-field-name field)
	     (alien-record-field-bits field))))

(def-alien-type-class (record :include mem-block)
  (kind :struct :type (member :struct :union))
  (name nil :type (or symbol null))
  (fields nil :type list))

(def-alien-type-translator struct (name &rest fields)
  (parse-alien-record-type :struct name fields))

(def-alien-type-translator union (name &rest fields)
  (parse-alien-record-type :union name fields))

(defun parse-alien-record-type (kind name fields)
  (if fields
      (let* ((old (and name (auxiliary-alien-type kind name)))
	     (result (if (or (null old)
			     (alien-record-type-fields old))
			 (make-alien-record-type :name name :kind kind)
			 old)))
	(when (and name (not (eq old result)))
	  (setf (auxiliary-alien-type kind name) result))
	(parse-alien-record-fields result fields)
	result)
      (if name
	  (or (auxiliary-alien-type kind name)
	      (setf (auxiliary-alien-type kind name)
		    (make-alien-record-type :name name :kind kind)))
	  (make-alien-record-type :kind kind))))

;;; PARSE-ALIEN-RECORD-FIELDS -- internal
;;;
;;; Used by parse-alien-type to parse the fields of struct and union
;;; types.  RESULT holds the record type we are paring the fields of,
;;; and FIELDS is the list of field specifications.
;;;
(defun parse-alien-record-fields (result fields)
  (declare (type alien-record-type result)
	   (type list fields))
  (let ((total-bits 0)
	(overall-alignment 1)
	(parsed-fields nil))
    (dolist (field fields)
      (destructuring-bind (var type &optional bits) field
	(declare (ignore bits))
	(let* ((field-type (parse-alien-type type))
	       (bits (alien-type-bits field-type))
	       (alignment (alien-type-alignment field-type))
	       (parsed-field
		(make-alien-record-field :type field-type
					 :name var)))
	  (push parsed-field parsed-fields)
	  (when (null bits)
	    (error "Unknown size: ~S"
		   (unparse-alien-type field-type)))
	  (when (null alignment)
	    (error "Unknown alignment: ~S"
		   (unparse-alien-type field-type)))
	  (setf overall-alignment (max overall-alignment alignment))
	  (ecase (alien-record-type-kind result)
	    (:struct
	     (let ((offset (align-offset total-bits alignment)))
	       (setf (alien-record-field-offset parsed-field) offset)
	       (setf total-bits (+ offset bits))))
	    (:union
	     (setf total-bits (max total-bits bits)))))))
    (let ((new (nreverse parsed-fields)))
      (setf (alien-record-type-fields result) new))
    (setf (alien-record-type-alignment result) overall-alignment)
    (setf (alien-record-type-bits result)
	  (align-offset total-bits overall-alignment))))

(def-alien-type-method (record :unparse) (type)
  `(,(case (alien-record-type-kind type)
       (:struct 'struct)
       (:union 'union)
       (t '???))
    ,(alien-record-type-name type)
    ,@(unless (member type *record-types-already-unparsed* :test #'eq)
	(push type *record-types-already-unparsed*)
	(mapcar #'(lambda (field)
		    `(,(alien-record-field-name field)
		      ,(%unparse-alien-type (alien-record-field-type field))
		      ,@(if (alien-record-field-bits field)
			    (list (alien-record-field-bits field)))))
		(alien-record-type-fields type)))))

;;; Test the record fields. The depth is limiting in case of cyclic
;;; pointers.
(defun record-fields-match (fields1 fields2 depth)
  (declare (type list fields1 fields2)
	   (type (mod 64) depth))
  (labels ((record-type-= (type1 type2 depth)
	     (and (eq (alien-record-type-name type1)
		      (alien-record-type-name type2))
		  (eq (alien-record-type-kind type1)
		      (alien-record-type-kind type2))
		  (= (length (alien-record-type-fields type1))
		     (length (alien-record-type-fields type2)))
		  (record-fields-match (alien-record-type-fields type1)
				       (alien-record-type-fields type2)
				       (1+ depth))))
	   (pointer-type-= (type1 type2 depth)
	     (let ((to1 (alien-pointer-type-to type1))
		   (to2 (alien-pointer-type-to type2)))
	       (if to1
		   (if to2
		       (type-= to1 to2 (1+ depth))
		       nil)
		   (null to2))))
	   (type-= (type1 type2 depth)
	     (cond ((and (alien-pointer-type-p type1)
			 (alien-pointer-type-p type2))
		    (or (> depth 10)
			(pointer-type-= type1 type2 depth)))
		   ((and (alien-record-type-p type1)
			 (alien-record-type-p type2))
		    (record-type-= type1 type2 depth))
		   (t
		    (alien-type-= type1 type2)))))
    (do ((fields1-rem fields1 (rest fields1-rem))
	 (fields2-rem fields2 (rest fields2-rem)))
	((or (eq fields1-rem fields2-rem)
	     (endp fields1-rem) (endp fields2-rem))
	 (eq fields1-rem fields2-rem))
      (let ((field1 (first fields1-rem))
	    (field2 (first fields2-rem)))
	(declare (type alien-record-field field1 field2))
	(unless (and (eq (alien-record-field-name field1)
			 (alien-record-field-name field2))
		     (eql (alien-record-field-bits field1)
			  (alien-record-field-bits field2))
		     (eql (alien-record-field-offset field1)
			  (alien-record-field-offset field2))
		     (let ((field1 (alien-record-field-type field1))
			   (field2 (alien-record-field-type field2)))
		       (type-= field1 field2 (1+ depth))))
	  (return nil))))))

(def-alien-type-method (record :type=) (type1 type2)
  (and (eq (alien-record-type-name type1)
	   (alien-record-type-name type2))
       (eq (alien-record-type-kind type1)
	   (alien-record-type-kind type2))
       (= (length (alien-record-type-fields type1))
	  (length (alien-record-type-fields type2)))
       (record-fields-match (alien-record-type-fields type1)
			    (alien-record-type-fields type2) 0)))


;;;; The FUNCTION and VALUES types.

(defvar *values-type-okay* nil)

(def-alien-type-class (function :include mem-block)
  "A Alien function that takes arguments of the specified ARG-TYPES and
   returns a result of type RESULT-TYPE.  Note that the only context where
   a function type is directly specified is in the argument to
   `alien-funcall' (as described in [alien-funcall].)  In all other
   contexts, functions are represented by function pointer types: (*
   (function ...))."
  (result-type (required-argument) :type alien-type)
  (arg-types (required-argument) :type list)
  (stub nil :type (or null function)))

(def-alien-type-translator function (result-type &rest arg-types)
  (make-alien-function-type
   :result-type (let ((*values-type-okay* t))
		  (parse-alien-type result-type))
   :arg-types (mapcar #'parse-alien-type arg-types)))

(def-alien-type-method (function :unparse) (type)
  `(function ,(%unparse-alien-type (alien-function-type-result-type type))
	     ,@(mapcar #'%unparse-alien-type
		       (alien-function-type-arg-types type))))

(def-alien-type-method (function :type=) (type1 type2)
  (and (alien-type-= (alien-function-type-result-type type1)
		     (alien-function-type-result-type type2))
       (= (length (alien-function-type-arg-types type1))
	  (length (alien-function-type-arg-types type2)))
       (every #'alien-type-=
	      (alien-function-type-arg-types type1)
	      (alien-function-type-arg-types type2))))


(def-alien-type-class (values)
  (values (required-argument) :type list))

(def-alien-type-translator values (&rest values)
  (unless *values-type-okay*
    (error "Cannot use values types here."))
  (let ((*values-type-okay* nil))
    (make-alien-values-type
     :values (mapcar #'parse-alien-type values))))

(def-alien-type-method (values :unparse) (type)
  `(values ,@(mapcar #'%unparse-alien-type
		     (alien-values-type-values type))))

(def-alien-type-method (values :type=) (type1 type2)
  (and (= (length (alien-values-type-values type1))
	  (length (alien-values-type-values type2)))
       (every #'alien-type-=
	      (alien-values-type-values type1)
	      (alien-values-type-values type2))))


;;;; Alien variables.

#[ Alien Variables

Both local (stack allocated) and external (C global) Alien variables are
supported.

[ Local Alien Variables    ]
[ External Alien Variables ]
]#

#[ Local Alien Variables

{function:alien:with-alien}
]#

#[ External Alien Variables

External Alien names are strings, and Lisp names are symbols.  When an
external Alien is represented using a Lisp variable, there must be a way to
convert from one name syntax into the other.  The macros `extern-alien',
`def-alien-variable' and `def-alien-routine' use this conversion heuristic:

  * Alien names are converted to Lisp names by uppercasing and replacing
    underscores with hyphens.

  * Conversely, Lisp names are converted to Alien names by lowercasing and
    replacing hyphens with underscores.

  * Both the Lisp symbol and Alien string names may be separately
    specified by using a list of the form:

        (alien-string lisp-symbol)

{function:alien:def-alien-variable}
{function:alien:extern-alien}
]#

;;; HEAP-ALIEN-INFO -- defstruct.
;;;
;;; Information describing a heap-allocated alien.
;;;
(defstruct (heap-alien-info
	    (:print-function %print-heap-alien-info)
	    (:make-load-form-fun :just-dump-it-normally))
  ;; The type of this alien.
  (type (required-argument) :type alien-type)
  ;; The form to evaluate to produce the SAP pointing to where in the heap
  ;; it is.
  (sap-form (required-argument)))
;;;
(defun %print-heap-alien-info (info stream depth)
  (declare (ignore depth))
  (print-unreadable-object (info stream :type t)
    (funcall (formatter "~S ~S")
	     stream
	     (heap-alien-info-sap-form info)
	     (unparse-alien-type (heap-alien-info-type info)))))

;;; LOCAL-ALIEN-INFO -- public defstruct.
;;;
;;; Information about local aliens.  The WITH-ALIEN macro builds one of these
;;; structures and local-alien and friends comunicate information about how
;;; that local alien is represented.
;;;
(defstruct (local-alien-info
	    (:print-function %print-local-alien-info)
	    (:make-load-form-fun :just-dump-it-normally)
	    (:constructor make-local-alien-info (&key type force-to-memory-p)))
  ;; The type of the local alien.
  (type (required-argument) :type alien-type)
  ;; T if this local alien must be forced into memory.  Using the ADDR macro
  ;; on a local alien will set this.
  (force-to-memory-p (or (alien-array-type-p type) (alien-record-type-p type))
		     :type (member t nil)))
;;;
(defun %print-local-alien-info (info stream depth)
  (declare (ignore depth))
  (print-unreadable-object (info stream :type t)
    (funcall (formatter "~:[~;(forced to stack) ~]~S")
	     stream
	     (local-alien-info-force-to-memory-p info)
	     (unparse-alien-type (local-alien-info-type info)))))

;;; GUESS-ALIEN-NAME-FROM-LISP-NAME -- internal.
;;;
;;; Make a string out of the symbol, converting all uppercase letters to
;;; lower case and hyphens into underscores.
;;;
(defun guess-alien-name-from-lisp-name (lisp-name)
  (declare (type symbol lisp-name))
  (nsubstitute #\_ #\- (string-downcase (symbol-name lisp-name))))

;;; GUESS-LISP-NAME-FROM-ALIEN-NAME -- internal.
;;;
;;; The opposite of GUESS-ALIEN-NAME-FROM-LISP-NAME.  Make a symbol out of the
;;; string, converting all lowercase letters to uppercase and underscores into
;;; hyphens.
;;;
(defun guess-lisp-name-from-alien-name (alien-name)
  (declare (type simple-string alien-name))
  (intern (nsubstitute #\- #\_ (string-upcase alien-name))))

;;; PICK-LISP-AND-ALIEN-NAMES -- internal.
;;;
;;; Extract the lisp and alien names from NAME.  If only one is given, guess
;;; the other.
;;;
(defun pick-lisp-and-alien-names (name)
  (etypecase name
    (string
     (values (guess-lisp-name-from-alien-name name) name))
    (symbol
     (values name (guess-alien-name-from-lisp-name name)))
    (list
     (unless (= (length name) 2)
       (error "Badly formed alien name."))
     (values (cadr name) (car name)))))

;;; DEF-ALIEN-VARIABLE -- public
;;;
(defmacro def-alien-variable (name type)
  "Define $name as an external alien variable of alien type $type.  The
   values of the $name and $type arguments are used directly ([FIX] vs
   evaluated values).

   Expect $name to be a list of a string holding the alien name and a
   symbol to use as the Lisp name.  If $name is just a symbol or string,
   then guess the other name from the one supplied.

   Global Alien variables are effectively \"global symbol macros\"; a
   reference to the variable fetches the contents of the external variable.
   Similarly, setting the variable stores new contents --- the new contents
   must be of the declared $type.

   For example, it is often necessary to read the global C variable
   \"errno\" to determine why a particular function call failed.  It is
   possible to define errno and make it accessible from Lisp with the
   following:

       (def-alien-variable \"errno\" int)

       ;; Now it is possible to get the value of the C variable errno
       ;; simply by referencing that Lisp variable:
       ;;
       (print errno)"
  (multiple-value-bind
      (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (with-auxiliary-alien-types
      (let ((alien-type (parse-alien-type type)))
	`(eval-when (compile load eval)
	   ,@(when *new-auxiliary-types*
	       `((%def-auxiliary-alien-types ',*new-auxiliary-types*)))
	   (%def-alien-variable ',lisp-name
				',alien-name
				',alien-type))))))

;;; %DEF-ALIEN-VARIABLE -- internal
;;;
;;; Do the actual work of DEF-ALIEN-VARIABLE.
;;;
(defun %def-alien-variable (lisp-name alien-name type)
  (setf (info variable kind lisp-name) :alien)
  (setf (info variable where-from lisp-name) :defined)
  (clear-info variable constant-value lisp-name)
  (setf (info variable alien-info lisp-name)
	(make-heap-alien-info :type type
			      :sap-form `(foreign-symbol-address
					  ',alien-name))))

;;; EXTERN-ALIEN -- public.
;;;
(defmacro extern-alien (name type)
  "Return an Alien with the specified $type which points to an externally
   defined value.

   Use the values of $name and $type directly (FIX vs evaluated values).
   $name may be specified either as a string or a symbol.  $type is an
   Alien type specifier.

   This function is `setf'able."
  (let ((alien-name (etypecase name
		      (symbol (guess-alien-name-from-lisp-name name))
		      (string name))))
    `(%heap-alien ',(make-heap-alien-info
		     :type (parse-alien-type type)
		     :sap-form `(foreign-symbol-address ',alien-name)))))

;;; WITH-ALIEN -- public.
;;;
(defmacro with-alien (bindings &body body)
  "Establish some local alien variables for the extent of $body.  Each
   $binding is of the form:
     var type [ allocation ] [ initial-value | external-name ]

   \"allocation\" should be one of:
     :LOCAL (the fallback)
       The alien is allocated on the stack, and has dynamic extent.
     :STATIC
       The alien is allocated on the heap, and has permanent extent.  The alien
       is allocated at load time, so the same piece of memory is used each time
       this form executes.
     :EXTERN
       \"var\" is established as a local name for the external alien given
       by \"external-name\".

   Bindings may be assigned with `setq' or `setf'.  This form is analogous
   to defining a local variable in C: additional storage is allocated, and
   the initial value is copied.

   `with-alien' also establishes a new scope for named structures and
   unions.  Any \"type\" specified for a variable may contain name
   structure or union types with the slots specified.  Within the lexical
   scope of the binding specifiers and body, a locally defined structure
   type foo can be referenced by its name using:

       (struct foo)"
  (with-auxiliary-alien-types
    (dolist (binding (reverse bindings))
      (destructuring-bind
	  (symbol type &optional (opt1 nil opt1p) (opt2 nil opt2p))
	  binding
	(let ((alien-type (parse-alien-type type)))
	  (multiple-value-bind
	      (allocation initial-value)
	      (if opt2p
		  (values opt1 opt2)
		  (case opt1
		    (:extern
		     (values opt1 (guess-alien-name-from-lisp-name symbol)))
		    (:static
		     (values opt1 nil))
		    (t
		     (values :local opt1))))
	    (setf body
		  (ecase allocation
		    #+nil
		    (:static
		     (let ((sap
			    (make-symbol (concatenate 'string "SAP-FOR-"
						      (symbol-name symbol)))))
		       `((let ((,sap (load-time-value (%make-alien ...))))
			   (declare (type system-area-pointer ,sap))
			   (symbol-macrolet
			    ((,symbol (sap-alien ,sap ,type)))
			    ,@(when initial-value
				`((setq ,symbol ,initial-value)))
			    ,@body)))))
		    (:extern
		     (let ((info (make-heap-alien-info
				  :type alien-type
				  :sap-form `(foreign-symbol-address
					      ',initial-value))))
		       `((symbol-macrolet
			  ((,symbol (%heap-alien ',info)))
			  ,@body))))
		    (:local
		     (let ((var (gensym))
			   (initval (if initial-value (gensym)))
			   (info (make-local-alien-info
				  :type alien-type)))
		       `((let ((,var (make-local-alien ',info))
			       ,@(when initial-value
				   `((,initval ,initial-value))))
			   (note-local-alien-type ',info ,var)
			   (multiple-value-prog1
			       (symbol-macrolet
				((,symbol (local-alien ',info ,var)))
				,@(when initial-value
				    `((setq ,symbol ,initval)))
				,@body)
			       (dispose-local-alien ',info ,var)
			       )))))))))))
    (verify-local-auxiliaries-okay)
    `(compiler-let ((*auxiliary-type-definitions*
		     ',(append *new-auxiliary-types*
			       *auxiliary-type-definitions*)))
       ,@body)))


;;;; Runtime C values that don't correspond directly to Lisp types.

;;; ALIEN-VALUE
;;;
;;; The defstruct for alien-value lives in struct.lisp 'cause it has to be
;;; real early in the cold-load order.
;;;
(declaim (freeze-type alien-value))
;;;
(defun %print-alien-value (value stream depth)
  (declare (ignore depth))
  (print-unreadable-object (value stream)
    (funcall (formatter "Alien ~S at #x~8,'0X")
	     stream
	     (unparse-alien-type (alien-value-type value))
	     (sap-int (alien-value-sap value)))))

(declaim (inline null-alien))
(defun null-alien (x)
  "Return true if $x (which must be an Alien pointer) is NULL, false
   otherwise."
  (zerop (sap-int (alien-sap x))))


(defmacro sap-alien (sap type)
  "Convert the system-area-pointer $sap to an Alien of $type.  Only
   evaluate $sap.  $type must be some Alien pointer, array or record type."
  (let ((alien-type (parse-alien-type type)))
    (if (eq (compute-alien-rep-type alien-type) 'system-area-pointer)
	`(%sap-alien ,sap ',alien-type)
	(error "Cannot make aliens of type ~S out of SAPs" type))))

(defun %sap-alien (sap type)
  (declare (type system-area-pointer sap)
	   (type alien-type type))
  (make-alien-value :sap sap :type type))

(defun alien-sap (alien)
  "Return a system-area-pointer pointing to $alien's data.  The type of
   $alien must be some Alien pointer, array or record type."
  (declare (type alien-value alien))
  (alien-value-sap alien))


;;;; Allocation/Deallocation of heap aliens.

;;; MAKE-ALIEN -- public.
;;;
(defmacro make-alien (type &optional size)
  "Return a dynamically allocated Alien of $type using the symbol $type
   directly.  The allocated memory may contain arbitrary junk.  If true,
   $size is an expression to evaluate to compute the size of the allocated
   value.

   There are two cases:

     * When $type is an array type, allocate an array of that type
       and return a pointer to it.  As a result the result must be changed
       to an array before being deref'ed to read or write elements:

         (defvar *foo* (make-alien (array char 10)))

         (type-of *foo*)
           => (alien (* (array (signed 8) 10)))

         (setf (deref (deref *foo*) 0) 10)
           => 10

       Use $size as the first dimension for the array.

     * When $type is any other type, then an object for that type is
       allocated, and a pointer to it is returned.  So (make-alien int)
       returns a (* int).  If $size is specified, then allocate a block of
       that many values, return a pointer to the first one.

   Allocate the memory using `malloc', so it can be passed to foreign
   functions which use `free'."
  (let ((alien-type (if (alien-type-p type) type (parse-alien-type type))))
    (multiple-value-bind
	(size-expr element-type)
	(if (alien-array-type-p alien-type)
	    (let ((dims (alien-array-type-dimensions alien-type)))
	      (cond
	       (size
		(unless dims
		  (error
		   "Cannot override the size of zero-dimensional arrays."))
		(when (constantp size)
		  (setf alien-type (copy-alien-array-type alien-type))
		  (setf (alien-array-type-dimensions alien-type)
			(cons (eval size) (cdr dims)))))
	       (dims
		(setf size (car dims)))
	       (t
		(setf size 1)))
	      (values `(* ,size ,@(cdr dims))
		      (alien-array-type-element-type alien-type)))
	    (values (or size 1) alien-type))
      (let ((bits (alien-type-bits element-type))
	    (alignment (alien-type-alignment element-type)))
	(unless bits
	  (error "Size of ~S unknown." (unparse-alien-type element-type)))
	(unless alignment
	  (error "Alignment of ~S unknown." (unparse-alien-type element-type)))
	`(%sap-alien (%make-alien (* ,(align-offset bits alignment)
				     ,size-expr))
		     ',(make-alien-pointer-type :to alien-type))))))

;;; %MAKE-ALIEN -- internal
;;;
;;; Allocate a block of memory at least BITS bits long and return a system
;;; area pointer to it.
;;;
(declaim (inline %make-alien))
(defun %make-alien (bits)
  (declare (type kernel:index bits) (optimize-interface (safety 2)))
  (alien-funcall (extern-alien "malloc" (function system-area-pointer unsigned))
		 (ash (the kernel:index (+ bits 7)) -3)))

;;; FREE-ALIEN -- public
;;;
(declaim (inline free-alien))
(defun free-alien (alien)
  "Free the storage pointed to by $alien.  $alien must have been allocated
   with `make-alien' or `malloc'."
  (alien-funcall (extern-alien "free" (function (values) system-area-pointer))
		 (alien-sap alien))
  ())


;;;; The SLOT operator

;;; SLOT-OR-LOSE -- internal.
;;;
;;; Find the field named SLOT, or die trying.
;;;
(defun slot-or-lose (type slot)
  (declare (type alien-record-type type)
	   (type symbol slot))
  (or (find slot (alien-record-type-fields type)
	    :key #'alien-record-field-name)
      (error "No slot named ~S in ~S" slot type)))

;;; SLOT -- public
;;;
;;; Extract the value from the named slot from the record alien.  If the
;;; alien is actually a pointer, then deref it first.
;;;
(defun slot (alien slot)
  "Extract the value of $slot from the an Alien struct or union.  If $alien
   is a pointer to a structure or union, then use the value pointed to
   automatically.

   This can be set with `setf' to assign a new value.

   $slot is evaluated, and can be a compile-time variable.  Only constant
   slot accesses are efficiently compiled."
  (declare (type alien-value alien)
	   (type symbol slot)
	   (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (slot (deref alien) slot))
      (alien-record-type
       (let ((field (slot-or-lose type slot)))
	 (extract-alien-value (alien-value-sap alien)
			      (alien-record-field-offset field)
			      (alien-record-field-type field)))))))

;;; %SET-SLOT -- public setf method
;;;
;;; Deposite the value in the specified slot of the record alien.  If the
;;; alien is really a pointer, deref it first.  The compiler uses this
;;; when it can't figure out anything better.
;;;
(defun %set-slot (alien slot value)
  (declare (type alien-value alien)
	   (type symbol slot)
	   (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (%set-slot (deref alien) slot value))
      (alien-record-type
       (let ((field (slot-or-lose type slot)))
	 (deposit-alien-value (alien-value-sap alien)
			      (alien-record-field-offset field)
			      (alien-record-field-type field)
			      value))))))
;;;
(defsetf slot %set-slot)

;;; %SLOT-ADDR -- internal
;;;
;;; Compute the address of the specified slot and return a pointer to it.
;;;
(defun %slot-addr (alien slot)
  (declare (type alien-value alien)
	   (type symbol slot)
	   (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (%slot-addr (deref alien) slot))
      (alien-record-type
       (let* ((field (slot-or-lose type slot))
	      (offset (alien-record-field-offset field))
	      (field-type (alien-record-field-type field)))
	 (%sap-alien (sap+ (alien-sap alien) (/ offset vm:byte-bits))
		     (make-alien-pointer-type :to field-type)))))))


;;;; The DEREF operator.

;;; DEREF-GUTS -- internal.
;;;
;;; Does most of the work of the different DEREF methods.  Returns two values:
;;; the type and the offset (in bits) of the refered to alien.
;;;
(defun deref-guts (alien indices)
  (declare (type alien-value alien)
	   (type list indices)
	   (values alien-type integer))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (when (cdr indices)
	 (error "Too many indices when derefing ~S: ~D"
		type
		(length indices)))
       (let ((element-type (alien-pointer-type-to type)))
	 (values element-type
		 (if indices
		     (* (align-offset (alien-type-bits element-type)
				      (alien-type-alignment element-type))
			(car indices))
		     0))))
      (alien-array-type
       (or (= (length indices) (length (alien-array-type-dimensions type)))
	   (error "Incorrect number of indices when derefing ~S: ~D"
		  type (length indices)))
       (labels ((frob (dims indices offset)
		  (if (null dims)
		      offset
		      (frob (cdr dims) (cdr indices)
			(+ (if (zerop offset)
			       0
			       (* offset (car dims)))
			   (car indices))))))
	 (let ((element-type (alien-array-type-element-type type)))
	   (values element-type
		   (* (align-offset (alien-type-bits element-type)
				    (alien-type-alignment element-type))
		      (frob (alien-array-type-dimensions type)
			indices 0)))))))))

;;; DEREF -- public
;;;
;;; Dereference the alien and return the results.
;;;
(defun deref (alien &rest indices)
  "Return the value pointed to by $alien, an Alien pointer or the value of
   an Alien array element.  If an array, use the $indices as the indices of
   the array element to access.  If a pointer, use $indices to offset
   $alien as with pointer arithmetic in the C language.

   `deref' can be set with `setf' to assign a new value."
  (declare (type alien-value alien)
	   (type list indices)
	   (optimize (inhibit-warnings 3)))
  (multiple-value-bind
      (target-type offset)
      (deref-guts alien indices)
    (extract-alien-value (alien-value-sap alien)
			 offset
			 target-type)))

;;; %SET-DEREF -- public setf method
;;;
(defun %set-deref (alien value &rest indices)
  (declare (type alien-value alien)
	   (type list indices)
	   (optimize (inhibit-warnings 3)))
  (multiple-value-bind
      (target-type offset)
      (deref-guts alien indices)
    (deposit-alien-value (alien-value-sap alien)
			 offset
			 target-type
			 value)))
;;;
(defsetf deref (alien &rest indices) (value)
  `(%set-deref ,alien ,value ,@indices))

;;; %DEREF-ADDR -- public
;;;
(defun %deref-addr (alien &rest indices)
  (declare (type alien-value alien)
	   (type list indices)
	   (optimize (inhibit-warnings 3)))
  (multiple-value-bind
      (target-type offset)
      (deref-guts alien indices)
    (%sap-alien (sap+ (alien-value-sap alien) (/ offset vm:byte-bits))
		(make-alien-pointer-type :to target-type))))


;;;; Accessing heap alien variables.

(defun %heap-alien (info)
  (declare (type heap-alien-info info)
	   (optimize (inhibit-warnings 3)))
  (extract-alien-value (eval (heap-alien-info-sap-form info))
		       0
		       (heap-alien-info-type info)))

(defun %set-heap-alien (info value)
  (declare (type heap-alien-info info)
	   (optimize (inhibit-warnings 3)))
  (deposit-alien-value (eval (heap-alien-info-sap-form info))
		       0
		       (heap-alien-info-type info)
		       value))
;;;
(defsetf %heap-alien %set-heap-alien)

(defun %heap-alien-addr (info)
  (declare (type heap-alien-info info)
	   (optimize (inhibit-warnings 3)))
  (%sap-alien (eval (heap-alien-info-sap-form info))
	      (make-alien-pointer-type :to (heap-alien-info-type info))))


;;;; Accessing local aliens.

(defun make-local-alien (info)
  (let* ((alien (eval `(make-alien ,(local-alien-info-type info))))
	 (alien-sap (alien-sap alien)))
    (finalize
     alien
     #'(lambda ()
	 (alien-funcall
	  (extern-alien "free" (function (values) system-area-pointer))
	  alien-sap)))
    alien))

(defun note-local-alien-type (info alien)
  (declare (ignore info alien))
  nil)

(defun local-alien (info alien)
  (declare (ignore info))
  (deref alien))

(defun %set-local-alien (info alien value)
  (declare (ignore info))
  (setf (deref alien) value))

(define-setf-method local-alien (&whole whole info alien)
  (let ((value (gensym))
	(info (if (and (consp info)
		       (eq (car info) 'quote))
		  (second info)
		  (error "Something is wrong; local-alien-info not found: ~S"
			 whole))))
    (values nil
	    nil
	    (list value)
	    (if c:*converting-for-interpreter*
		`(%set-local-alien ',info ,alien ,value)
		`(if (%local-alien-forced-to-memory-p ',info)
		     (%set-local-alien ',info ,alien ,value)
		     (setf ,alien
			   (deport ,value ',(local-alien-info-type info)))))
	    whole)))

(defun %local-alien-forced-to-memory-p (info)
  (local-alien-info-force-to-memory-p info))

(defun %local-alien-addr (info alien)
  (declare (type local-alien-info info))
  (unless (local-alien-info-force-to-memory-p info)
    (error "~S isn't forced to memory.  Something went wrong." alien))
  alien)

(defun dispose-local-alien (info alien)
  (declare (ignore info))
  (cancel-finalization alien)
  (free-alien alien))


;;;; The ADDR macro.

(defmacro addr (expr &environment env)
  "Return an Alien pointer to the data addressed by $expr, which must be a
   call to `slot' or `deref', or a reference to an Alien variable."
  (let ((form (macroexpand expr env)))
    (or (typecase form
	  (cons
	   (case (car form)
	     (slot
	      (cons '%slot-addr (cdr form)))
	     (deref
	      (cons '%deref-addr (cdr form)))
	     (%heap-alien
	      (cons '%heap-alien-addr (cdr form)))
	     (local-alien
	      (let ((info
		     (let ((info-arg (second form)))
		       (and (consp info-arg)
			    (eq (car info-arg) 'quote)
			    (second info-arg)))))
		(unless (local-alien-info-p info)
		  (error "Something is wrong, local-alien-info not found: ~S"
			 form))
		(setf (local-alien-info-force-to-memory-p info) t))
	      (cons '%local-alien-addr (cdr form)))))
	  (symbol
	   (let ((kind (info variable kind form)))
	     (when (eq kind :alien)
	       `(%heap-alien-addr ',(info variable alien-info form))))))
	(error "~S is not a valid L-value" form))))


;;;; The CAST macro.

(defmacro cast (alien type)
  "Convert $alien to a new Alien of $type.  Only evaluate $alien.

   Both types must be Alien array, pointer or function types.

   The result does refer to the same data bits, however, an `eq' comparison
   to the original will fail."
  `(%cast ,alien ',(parse-alien-type type)))

(defun %cast (alien target-type)
  (declare (type alien-value alien)
	   (type alien-type target-type)
	   (optimize-interface (safety 2))
	   (optimize (inhibit-warnings 3)))
  (if (or (alien-pointer-type-p target-type)
	  (alien-array-type-p target-type)
	  (alien-function-type-p target-type))
      (let ((alien-type (alien-value-type alien)))
	(if (or (alien-pointer-type-p alien-type)
		(alien-array-type-p alien-type)
		(alien-function-type-p alien-type))
	    (naturalize (alien-value-sap alien) target-type)
	    (error "~S cannot be casted." alien)))
      (error "Cannot cast to alien type ~S" (unparse-alien-type target-type))))


;;;; The ALIEN-SIZE macro.

(defmacro alien-size (type &optional (units :bits))
  "Return the size of the alien type TYPE.  UNITS specifies the units to
   use and can be either :BITS, :BYTES, or :WORDS."
  (let* ((alien-type (parse-alien-type type))
	 (bits (alien-type-bits alien-type)))
    (if bits
	(values (ceiling bits
			 (ecase units
			   (:bits 1)
			   (:bytes vm:byte-bits)
			   (:words vm:word-bits))))
	(error "Unknown size for alien type ~S."
	       (unparse-alien-type alien-type)))))


;;;; Naturalize, deport, extract-alien-value, deposit-alien-value

(defun naturalize (alien type)
  (declare (type alien-type type))
  (funcall (coerce (compute-naturalize-lambda type) 'function)
	   alien type))

(defun deport (value type)
  (declare (type alien-type type))
  (funcall (coerce (compute-deport-lambda type) 'function)
	   value type))

(defun extract-alien-value (sap offset type)
  (declare (type system-area-pointer sap)
	   (type unsigned-byte offset)
	   (type alien-type type))
  (funcall (coerce (compute-extract-lambda type) 'function)
	   sap offset type))

(defun deposit-alien-value (sap offset type value)
  (declare (type system-area-pointer sap)
	   (type unsigned-byte offset)
	   (type alien-type type))
  (funcall (coerce (compute-deposit-lambda type) 'function)
	   sap offset type value))


;;;; alien-funcall, def-alien-function

(defun alien-funcall (alien &rest args)
  "The foreign function call primitive: call $alien with arguments $args
   and return its value.

   $alien is an arbitrary run-time expression; `extern-alien' and
   `def-alien-routine' are for calling constant functions.

   The type of $alien must be (alien (function ...)) or (alien (* (function
   ...))), [Alien Type Specifiers].  Use the function type to determine how
   to call the function (as through it was declared with a prototype).  The
   type can be variable at compile time; known-type calls are efficiently
   compiled.  The implementation lacks

     * structure type return values
     * passing of structures by value."
  (declare (type alien-value alien))
  (let ((type (alien-value-type alien)))
    (typecase type
      (alien-pointer-type
       (apply #'alien-funcall (deref alien) args))
      (alien-function-type
       (unless (= (length (alien-function-type-arg-types type))
		  (length args))
	 (error "Wrong number of arguments for ~S~%Expected ~D, got ~D."
		type
		(length (alien-function-type-arg-types type))
		(length args)))
       (let ((stub (alien-function-type-stub type)))
	 (unless stub
	   (setf stub
		 (let ((fun (gensym))
		       (parms (loop repeat (length args) collect (gensym))))
		   (compile nil
			    `(lambda (,fun ,@parms)
			       (declare (type (alien ,type) ,fun))
			       (alien-funcall ,fun ,@parms)))))
	   (setf (alien-function-type-stub type) stub))
	 (apply stub alien args)))
      (t
       (error "~S is not an alien function." alien)))))

(defmacro def-alien-routine (name result-type &rest args)
  "def-alien-routine NAME RESULT-TYPE {(ARG-NAME ARG-TYPE [STYLE])}*

   Define a foreign interface function for the routine named $name,
   which may be either a string, symbol or list of the foreign name and the
   Lisp name, of the form (string symbol).  If only one name is
   specified, the other is automatically derived ([External Alien Variables]).

   $result-type is the Alien type for the function return value.  \"void\"
   may be used to specify a void function.

   This macro is a convenience for automatically generating Lisp interfaces to
   simple foreign functions.  The primary feature is the parameter style
   specification, which translates the C pass-by-reference idiom into additional
   return values.

   The remaining forms specifiy arguments that are passed to the routine.
   $arg-name is a symbol that names the argument, primarily for
   documentation.  $arg-type is the C-Type of the argument.  $style
   specifies the way that the argument is passed.  The semantics of the
   actual call are the same as for `alien-funcall'.  $style should be one
   of the following:

   :in
        An :in argument is simply passed by value.  The value to be passed is
        obtained from argument(s) to the interface function.

   :out
        The specified argument type must be a pointer to a fixed sized
        object.  A pointer to a preallocated object is passed to the
        routine, and the the object is accessed on return, with the value
        being returned from the interface function.  :out and :in-out
        cannot be used with pointers to arrays, records or functions.

   :copy
        As for :in, with the addition that the argument values are stored
        on the stack, and a pointer to the object is passed instead of the
        value itself.

   :in-out
        A combination of :out and :copy.  A pointer to the argument is
        passed, with the object being initialized from the supplied
        argument and the return value being determined by accessing the
        object on return.

   Any efficiency-critical foreign interface function should be inline
   expanded by preceding `def-alien-routine' with:

       (declaim (inline lisp-name))

   In addition to saving the Lisp call overhead, this allows pointers,
   word-integers and floats to be passed using non-descriptor representations,
   which saves consing ([non-descriptor].)"
  (multiple-value-bind
      (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (collect ((docs) (lisp-args) (arg-types) (alien-vars)
	      (alien-args) (results))
      (dolist (arg args)
	(if (stringp arg)
	    (docs arg)
	    (destructuring-bind (name type &optional (style :in)) arg
	      (or (member style '(:in :copy :out :in-out))
		  (error "Bogus argument style ~S in ~S." style arg))
	      (or (eq style :out)
		  (lisp-args name))
	      (and (member style '(:out :in-out))
		   (typep (parse-alien-type type) 'alien-pointer-type)
		   (error "Attempt to use :out or :in-out on pointer-like type:~%  ~S"
			  type))
	      (cond ((eq style :in)
		     (arg-types type)
		     (alien-args name))
		    (t
		     (arg-types `(* ,type))
		     (if (eq style :out)
			 (alien-vars `(,name ,type))
			 (alien-vars `(,name ,type ,name)))
		     (alien-args `(addr ,name))))
	      (when (or (eq style :out) (eq style :in-out))
		(results name)))))
      `(defun ,lisp-name ,(lisp-args)
	 ,@(docs)
	 (with-alien
	     ((,lisp-name (function ,result-type ,@(arg-types))
			  :extern ,alien-name)
	      ,@(alien-vars))
	     ,(if (alien-values-type-p result-type)
		  (let ((temps (loop
				 repeat (length (alien-values-type-values
						 result-type))
				 collect (gensym))))
		    `(multiple-value-bind
			 ,temps
			 (alien-funcall ,lisp-name ,@(alien-args))
		       (values ,@temps ,@(results))))
		  `(values (alien-funcall ,lisp-name ,@(alien-args))
			   ,@(results))))))))


#[ Alien Data Structure Example

Now that we have Alien types, operations and variables, we can manipulate
foreign data structures.  This C declaration

    struct foo {
	int a;
	struct foo *b[100];
    };

can be translated into this Alien type

    (def-alien-type nil
      (struct foo
	(a int)
	(b (array (* (struct foo)) 100))))

With this definition, the following C expression

    struct foo f;
    f.b[7].a

in this way

    (with-alien ((f (struct foo)))
      (slot (deref (slot f 'b) 7) 'a)
      ;;
      ;; Do something with f...
      )

Or consider this example of an external C variable and some accesses:

    struct c_struct {
	    short x, y;
	    char a, b;
	    int z;
	    c_struct *n;
    };

    extern struct c_struct *my_struct;

    my_struct->x++;
    my_struct->a = 5;
    my_struct = my_struct->n;

which can be manipulated in Lisp like this:

    (def-alien-type nil
      (struct c-struct
	      (x short)
	      (y short)
	      (a char)
	      (b char)
	      (z int)
	      (n (* c-struct))))

    (def-alien-variable "my_struct" (* c-struct))

    (incf (slot my-struct 'x))
    (setf (slot my-struct 'a) 5)
    (setq my-struct (slot my-struct 'n))
]#


#[ Step-by-Step Alien Example

This section presents a complete example of an interface to a somewhat
complicated C function.  This example should give a fairly good idea of how to
get the effect you want for almost any kind of C function.  Suppose you have
the following C function which you want to be able to call from Lisp in the
file "test.c":

    struct c_struct
    {
      int x;
      char *s;
    };

    struct c_struct *c_function (i, s, r, a)
        int i;
        char *s;
        struct c_struct *r;
        int a[10];
    {
      int j;
      struct c_struct *r2;

      printf("i = %d\n", i);
      printf("s = %s\n", s);
      printf("r->x = %d\n", r->x);
      printf("r->s = %s\n", r->s);
      for (j = 0; j < 10; j++) printf("a[%d] = %d.\n", j, a[j]);
      r2 = (struct c_struct *) malloc (sizeof(struct c_struct));
      r2->x = i + 5;
      r2->s = "A C string";
      return(r2);
    };

It is possible to call this function from Lisp using the file \file{test.lisp}
whose contents is:

    ;;; -*- Package: test-c-call -*-
    (in-package "TEST-C-CALL")
    (use-package "ALIEN")
    (use-package "C-CALL")

    ;;; Define the record c-struct in Lisp.
    (def-alien-type nil
        (struct c-struct
                (x int)
                (s c-string)))

    ;;; Define the Lisp function interface to the C routine.  It returns a
    ;;; pointer to a record of type c-struct.  It accepts four parameters:
    ;;; i, an int; s, a pointer to a string; r, a pointer to a c-struct
    ;;; record; and a, a pointer to the array of 10 ints.
    ;;;
    ;;; The INLINE declaration eliminates some efficiency notes about heap
    ;;; allocation of Alien values.
    (declaim (inline c-function))
    (def-alien-routine c-function
        (* (struct c-struct))
      (i int)
      (s c-string)
      (r (* (struct c-struct)))
      (a (array int 10)))

    ;;; A function which sets up the parameters to the C function and
    ;;; actually calls it.
    (defun call-cfun ()
      (with-alien ((ar (array int 10))
                   (c-struct (struct c-struct)))
        (dotimes (i 10)                     ; Fill array.
          (setf (deref ar i) i))
        (setf (slot c-struct 'x) 20)
        (setf (slot c-struct 's) "A Lisp String")

        (with-alien ((res (* (struct c-struct))
                          (c-function 5 "Another Lisp String" (addr c-struct) ar)))
          (format t "Returned from C function.~%")
          (multiple-value-prog1
              (values (slot res 'x)
                      (slot res 's))
            ;;
            ;; Deallocate result \i{after} we are done using it.
            (free-alien res)))))

To execute the above example, it is necessary to compile the C routine as
follows:

    cc -c test.c

In order to enable incremental loading with some linkers, you may need to say:

    cc -G 0 -c test.c

Once the C code has been compiled, you can start up Lisp and load it in:

    %lisp
    ;;; Lisp should start up with its normal prompt.

    ;;; Compile the Lisp file.  This step can be done separately.  You don't have
    ;;; to recompile every time.
    [ (compile-file "test.lisp")

    ;;; Load the foreign object file to define the necessary symbols.  This must
    ;;; be done before loading any code that refers to these symbols.  next block
    ;;; of comments are actually the output of LOAD-FOREIGN.  Different linkers
    ;;; will give different warnings, but some warning about redefining the code
    ;;; size is typical.
    [ (load-foreign "test.o")

    ;;; Running library:load-foreign.csh...
    ;;; Loading object file...
    ;;; Parsing symbol table...
    Warning:  "_gp" moved from #x00C082C0 to #x00C08460.

    Warning:  "end" moved from #x00C00340 to #x00C004E0.

    ;;; o.k. now load the compiled Lisp object file.
    [ (load "test")

    ;;; Now we can call the routine that sets up the parameters and calls the C
    ;;; function.
    [ (test-c-call ]call-cfun)

    ;;; The C routine prints the following information to standard output.
    i = 5
    s = Another Lisp string
    r->x = 20
    r->s = A Lisp string
    a[0] = 0.
    a[1] = 1.
    a[2] = 2.
    a[3] = 3.
    a[4] = 4.
    a[5] = 5.
    a[6] = 6.
    a[7] = 7.
    a[8] = 8.
    a[9] = 9.
    ;;; Lisp prints out the following information.
    Returned from C function.
    ;;; Return values from the call to test-c-call ]call-cfun.
    10
    "A C string"
    *

If any of the foreign functions do output, they should not be called from
within the editor.  Depending on the situation, various strange behavior
occurs.  Under X, the output goes to the window in which Lisp was started;
on a terminal, the output will overwrite the editor screen image; in an
editor slave, standard output is "/dev/null" by default, so any output is
discarded.
]#
