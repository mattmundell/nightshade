;;; Environment query functions, documentation and dribble.

(in-package "LISP")
(export '(function-args function-name
	  documentation *features* variable room
	  lisp-implementation-type version build-time machine-type
	  machine-version machine-instance software-type software-version
	  short-site-name long-site-name current-user dribble compiler-macro
	  user-email user-full-name user-name))

(in-package "SYSTEM")
(export '(*software-type* *short-site-name* *long-site-name*))

(in-package "EXT")
(export 'featurep)

(in-package "LISP")

;;; FIX Where should this go?  ~Build.lisp
#[ Directory Layout

Typically the source and build directories on a development system are
layed out as below.  [System Building] details building the system, and
[Installation] details installing from a prepackaged distribution.

== The Layout ==

The source-file assembler, with machine specific subdirectories of assembly
code for various architectures.

  * src/assembly/

  * src/assembly/alpha/

  * src/assembly/hppa/

  * src/assembly/mips/

  * src/assembly/rt/

  * src/assembly/sparc/

  * src/assembly/x86/

The Lisp code for the runtime system and utilities.

  * src/code/

The compiler.  Includes architecture-specific subdirectories holding
backends for various machines.  The "generic" subdirectory holds code that
is shared across most backends.  [Compiler Overview] details which files in
"src/compiler" hold which phases of the compiler.

  * src/compiler/

  * src/compiler/alpha/

  * src/compiler/generic/

  * src/compiler/hppa/

  * src/compiler/mips/

  * src/compiler/rt/

  * src/compiler/sparc/

  * src/compiler/x86/

The [Editor].

  * src/ed/

The C runtime system code and the low-level Lisp inspector.

  * src/lisp/

System building tools and shell scripts.

  * src/tools/

Packages.

  * src/packages/

Tests.

  * src/tests/

The current build.  Compiled versions of the source files from the
associated src/ directories are placed in these directories by the build
process.

  * 1/

  * 1/assembly/

  * 1/assembly/x86/

  * 1/assembly/*/

  * 1/code/

  * 1/compiler/

  * 1/ed/

  * 1/lisp/

The current builder.  This is the system that is used to build the system
in build/.  The builder is periodically replaced with the build.

  * 0/

  * 0/assembly/

  * 0/assembly/x86/

  * 0/assembly/*/

  * 0/code/

  * 0/compiler/

  * 0/ed/

  * 0/lisp/

A link to the current build.

  * build --> 1

A link to the current builder.

  * builder --> 0

== Manifests ==

Each directory contains a file ".manifest" which lists files and
directories in the directory.
]#


;;;; Documentation.

;;; FIX cobbled from stuff in describe.lisp.
(defun function-name (x)
  "Return the name of function X."
  (case (kernel:get-type x)
    (#.vm:closure-header-type
     (kernel:%function-name (%closure-function x)))
    ((#.vm:function-header-type #.vm:closure-function-header-type)
     (kernel:%function-name x))
    (#.vm:funcallable-instance-header-type
     (typecase x
       (kernel:byte-function
	(c::byte-function-name x))
       (kernel:byte-closure
	(c::byte-function-name (byte-closure-function x)))
       (eval:interpreted-function
	(multiple-value-bind
	    (exp closure-p dname)
	    (eval:interpreted-function-lambda-expression x)
	  (declare (ignore exp closure-p))
	  dname))
       (t ;; funcallable-instance
	(kernel:%function-name
	 (kernel:funcallable-instance-function x)))))))

;;; FIX cobbled from stuff in describe.lisp.
(defun function-args (x)
  "Return the argument list of function X."
  (case (get-type x)
    (#.vm:closure-header-type
     (%function-arglist x))
    ((#.vm:function-header-type #.vm:closure-function-header-type)
     (%function-arglist x))
    (#.vm:funcallable-instance-header-type
     (typecase x
       (kernel:byte-function
	"FIX (describe-function-byte-compiled x kind name)")
       (kernel:byte-closure
	"FIX (describe-function-byte-compiled (byte-closure-function x)")
       (eval:interpreted-function
	(eval:interpreted-function-arglist x))
       (t
	"FIX (describe-instance x :funcallable-instance)")))
    (t "")))

(defun function-doc (x &optional name)
  (let ((name (or name (function-name x))))
    (when (and name (typep name '(or symbol cons)))
      (values (info function documentation name)))))

(defun documentation (x doc-type)
  "Return the documentation string of $doc-type for $x if one exists, else
   ().  System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE, SETF, and
   T."
  (flet ((try-cmucl-random-doc (x doc-type)
	   (declare (symbol doc-type))
	   (cdr (assoc doc-type
		       (values (info random-documentation stuff x))))))
    (case doc-type
      (variable
       (typecase x
	 (symbol (values (info variable documentation x)))))
      (function
       (typecase x
	 (symbol (values (info function documentation x)))
	 (function (function-doc x))
	 (list ;; Must be '(setf symbol)
	  (values (info function documentation (cadr x))))))
      (structure
       (typecase x
	 (symbol (when (eq (info type kind x) :instance)
		   (values (info type documentation x))))))
      (type
       (typecase x
	 (structure-class (values (info type documentation (class-name x))))
	 (t (and (typep x 'symbol) (values (info type documentation x))))))
      (setf (info setf documentation x))
      ((t)
       (typecase x
	 (function (function-doc x))
	 (package (package-doc-string x))
	 (structure-class (values (info type documentation (class-name x))))
	 (symbol (try-cmucl-random-doc x doc-type))))
      (t
       (typecase x
	 (symbol (try-cmucl-random-doc x doc-type)))))))

(defun (setf documentation) (string name doc-type)
  (case doc-type
    (variable (setf (info variable documentation name) string))
    (function (setf (info function documentation name) string))
    (structure
     (unless (eq (info type kind name) :instance)
       (error "~S is not the name of a structure type." name))
     (setf (info type documentation name) string))
    (type (setf (info type documentation name) string))
    (setf (setf (info setf documentation name) string))
    (t
     (let ((pair (assoc doc-type (info random-documentation stuff name))))
       (if pair
	   (setf (cdr pair) string)
	   (push (cons doc-type string)
		 (info random-documentation stuff name))))))
  string)

(defvar *features* '(:common :cmu :new-compiler :cltl2 :lisp :ansi-cl
			     :draft-ansi-cl :x3j13 :ieee-floating-point)
  "Holds a list of symbols that describe features provided by the
   implementation.")

(defun featurep (x)
  "If X is an atom, see if it is present in *FEATURES*.  Also handle
   arbitrary combinations of atoms using NOT, AND, OR."
  (if (consp x)
      (case (car x)
	((:not not) (not (featurep (cadr x))))
	((:and and) (every #'featurep (cdr x)))
	((:or or) (some #'featurep (cdr x)))
	(t
	 (error "Unknown operator in feature expression: ~S." x)))
      (not (null (memq x *features*)))))


;;;; Other Environment Inquiries.

(defun lisp-implementation-type ()
  "Returns a string describing the implementation type."
  "Nightshade")

(defun version ()
  "Return a string describing the system version."
  *version*)

(defun build-time ()
  "Return the time at which the core was built."
  *build-time*)

(defun machine-instance ()
  "Returns a string giving the name of the local machine."
  (unix:unix-gethostname))

(defvar *software-type* "Unix"
  "The value of SOFTWARE-TYPE.  Set in FOO-os.lisp.")

(defun software-type ()
  "Returns a string describing the supporting software."
  *software-type*)

(defvar *short-site-name* "Short site name"
  "The value of SHORT-SITE-NAME.  Set in library:site-init.lisp.")

(defun short-site-name ()
  "Returns a string with the abbreviated site name."
  *short-site-name*)

(defvar *long-site-name* "Long site name"
  "The value of LONG-SITE-NAME.  Set in library:site-init.lisp.")

(defun long-site-name ()
  "Returns a string with the long form of the site name."
  *long-site-name*)


;;;; Users.  FIX mv somewhere appropriate

(defun current-user ()
  "Return the ID of the current user."
  (unix:unix-getuid))

(defun user-name (&optional (id (current-user)))
  "Return the string name of user $id."
  (unix:unix-user-name id))

(defun user-full-name (&optional (id (current-user)))
  "Return the full string name of user $id."
  (unix:unix-user-full-name id))

(defun user-email (&optional (id (current-user)))
  "Return the email address of user $id."
  (format () "~A@~A" (unix:unix-user-name id) (machine-instance)))

(defun prompt-long ()
  "Return a long format prompt string."
  (format () "~A ~A@~A~A> "
	  (package-name (current-package))
	  (user-name)
	  (machine-instance)
	  (let ((dir (namestring (namify (current-directory))))
		(home (namestring (namify (truename "home:")))))
	    (if (and (>= (length dir) (length home))
		     (string= dir home :end1 (length home)))
		(concatenate 'string ":" (subseq dir (length home)))
		dir))))


;;;; Dribble stuff.

;;; Each time we start dribbling to a new stream, we put it in
;;; *dribble-stream*, and push a list of *dribble-stream*, *standard-input*,
;;; *standard-output* and *error-output* in *previous-streams*.
;;; *standard-output* and *error-output* is changed to a broadcast stream that
;;; broadcasts to *dribble-stream* and to the old values of the variables.
;;; *standard-input* is changed to an echo stream that echos input from the old
;;; value of standard input to *dribble-stream*.
;;;
;;; When dribble is called with no arguments, *dribble-stream* is closed,
;;; and the values of *dribble-stream*, *standard-input*, and
;;; *standard-output* are poped from *previous-streams*.

(defvar *previous-streams* nil)
(defvar *dribble-stream* nil)

(defun dribble (&optional pathname &key (if-exists :append))
  "If PATHNAME is true, open the file and sends a record of further I/O to
   that file; otherwise close the dribble file and quit logging."
  (cond (pathname
	 (let* ((new-dribble-stream
		 (open pathname :direction :output :if-exists if-exists
		       :if-does-not-exist :create))
		(new-standard-output
		 (make-broadcast-stream *standard-output* new-dribble-stream))
		(new-error-output
		 (make-broadcast-stream *error-output* new-dribble-stream))
		(new-standard-input
		 (make-echo-stream *standard-input* new-dribble-stream)))
	   (push (list *dribble-stream* *standard-input* *standard-output*
		       *error-output*)
		 *previous-streams*)
	   (setf *dribble-stream* new-dribble-stream)
	   (setf *standard-input* new-standard-input)
	   (setf *standard-output* new-standard-output)
	   (setf *error-output* new-error-output)))
	((null *dribble-stream*)
	 (error "Not currently dribbling."))
	(t
	 (let ((old-streams (pop *previous-streams*)))
	   (close *dribble-stream*)
	   (setf *dribble-stream* (first old-streams))
	   (setf *standard-input* (second old-streams))
	   (setf *standard-output* (third old-streams))
	   (setf *error-output* (fourth old-streams)))))
  (values))


;;;; FIX Where should this be?

#[ Lisp

FIX This is still very rough.

Lisp is a language for defining computer programs.  There are many dialects
and variations of Lisp.  Nightshade Lisp is a modified subset of Common
Lisp.

Lisp centers around the evaluation of functions.  In Lisp a program is
described by a set of lists.  Each list describes the evaluation of a
function, optionally on a number of arguments.  Evaluation of a function
produces a return value and can have side-effects.  Side effects include
creating or modifying functions, macros, data and types.  For example, the
list

    (+ 1 2)

describes the evaluation of the + function on two arguments.  The return
value is 3.

FIX Where does the language end and the library begin?
   primitives


[ Read         ]      Reading programs into the system.
[ Evaluate     ]      Applying a function to some values.
[ Print        ]      Printing Lisp.
[ Comments     ]      Annotating programs.

== Primitives, Core of Lang ==

like mathematical/formal basis

[ Wandering All    ]
[ Wandering Struct ]

== Data ==

Data Types

[ Lists       ]
[ Symbol      ]  A named reference to a value (i.e. a variable)
[ Binding     ]  Binding symbols to values: let, let*, multiple-value-bind

    FIX
    lexical,dynamic scope
    extent
    dynamic and static typing

[ Binary            ]  t ()
[ Characters (Lisp) ]
[ Number            ]
[ Structure         ]
[ Sequence          ]  cons  vector  string
[ Other Types       ]  ref to pkg sys, libs   eg string tables

[ Reflection        ]  type-of, typep

closures

== Function ==

[ Functions          ]  &optional &rest &key &allow-other-keys &aux &body &whole &environment
[ Macros             ]  a type? part of the reader? part of evaluation?
[ Returning          ]  return return-from values prog1
[ Local Functions    ]  flet

[ Comparison         ]  Comparing values: eq eql equal =
[ Logical operations ]  or and
[ Bit manipulation   ]  ash log...

== Flow of Control ==

stack

[ Blocks          ]  block, progn
[ Conditionals    ]  A choice: cond if fi when fi* case case=
[ Iteration       ]  Looping and recursion: while until loop iterate

    dolist dotimes tagbody
    apply
    map map-into mapc mapcar mapcan mapl maplist mapcon
    reduce

[ Labels          ]  label
[ Exceptions      ]  catch throw error
[ Conditions      ]  signal handler-bind handler-case

== More ==

[ Packages (lisp) ]  Grouping functions and variables.
[ Features        ]  lang?
[ Assertions      ]  check-type, assert

[ Summary of Key/All Functions ]

== ~ On Top of Lang ==

[ Mathematics     ]
[ Arithmetic      ]

    if this is to mostly lists of functions maybe just:

[ package:arith ]  Basic arithmetic.
[ package:maths ]  More complex mathematics.

[ Packages ]  (from [system usage])

    maybe this entire section can be covered by the package interface,
    or filled in from the package list?
    should be only few key libraries?
]#

#[ Wandering All

    The language is a set of descriptions of state.
    The language can describe a state, including all possible changes
    that the state can undergo.
        ; dimension for function vibe, needs to think through handling if
        ; think should focus more on values than objects
    The language can describe the evaluation of objects.  Objects can be
    symbols or structures.  A structure is a grouping of one or more
        ; or lists which evaluate as functions, w stack-like arg eval
    optionally named objects.  A symbol is a reference to another object.
    Evaluation of a symbol produces the referred object.  Evaluation of
    a structure produces the structure.

        ;; ~ structures and symbols?
    The integers are structures of two symbols, universe and empty.  For
    example 1 is a structure of a structure of a symbol which is universe,
    and 3 is a structure of a structure of two symbols both universe.

        ;; zero dimension
        (defstruct empty)    ; the empty structure, given name empty
        (defstruct universe (:include universe))  ; the universe strct given name universe
        (defstruct)             ; empty?
        (defstruct (:include))  ; universe? the empty structure included in itself


             t is a function that returns itself
             if each slot is thought of as a function
             then structures are ~just collections of functions
                 what about first arg?


        ;; one dimension
        (defstruct fixnum
          ;; FIX how build in continuity?
          ;;        prhps integers (combinations) subset of continuity (like generalised below)
          (0 (type (or empty universe)))
          (1 (type (or empty universe)))
          ...
          (31 (type (or empty universe))))

        (defstruct combination
          ;; FIX how build in continuity?
          (0 (type (or empty universe)))
          (1 (type (or empty universe)))
          ...
          (n (type (or empty universe)))) ; FIX how n?
        (defstruct (fixnum (:include (combination 32))))
        (defstruct (integer (:include (combination n?))))
        (defstruct ratio
          numerator
          denominator)
        (defstruct generalised
          (structure :type (or () list))
          (closure))
        ;; then define ration to include generalised with closure being a / function?

        ;; two dimensions
        (defstruct square
          (length 0 :type fixnum))     ; Length of edge.
        ;;
        (defstruct user
          (a (type int))
          b
          (c (type (or empty universe)))) ; FIX ?

             how does this relate to functions?
                 functions are a structure?

    Sequences of evaluation can be shared in functions.
]#

#[ Wandering Struct

layout
class
        structure
        built-in

value
   type
       type of value == structure of value
               type comparison based on structural layout
                           component (field) names?
                   partial equivalence, compatibility
                           somehow every type equiv?
                                   equiv to empty type?  (defstruct empty)
       structure of value defined by struct def
                   type of struct def: struct def
           type equivalence based on components of structure
                   need to name structure defs?
                           to create,include them
                                   just a variable holding the structure
                                           structure slot on symbol?
           structural inclusion
                   just a definition mechanism?
                   need every type include a root structure? maybe empty
   a value property, perhaps "class", with a name
       many classes can have the same structure
]#


#|
FIX Where does the language end and the library begin?
   primitives
       universe, empty, structure, evaluation
       processor overview
   maybe anything defined in terms of prim is lib
FIX Where does the language end and the system begin?
   reading, evaluation functions, compiling
   maybe system subset of lib
|#
#[ Ni

Ni is a language for defining the behaviour of a computer.  As such, Ni
defines the state of the computer and the change in that state over time.
Ni is a modified subset of Common Lisp.

== Data ==

Data defines the state of the computer.

[ Read              ]      Reading data into the system.
   needs function

structure   empty  universe

[ Structure  ]  Defining data.

Predefined structure.

[ List      ]  A chain of values.
[ Character ]
[ Number    ]
[ Symbol    ]  A named reference to a value (i.e. a variable)
[ Sequence  ]  Vectors, strings.

Predefined values.

[ True and False     ]  #t #f

== Evaluation ==

Evaluation changes the state of the computer over time.

universe can become empty
universe can become universe
empty can become universe
empty can become empty

[ Function     ]      data structure function  list arguments result
[ Evaluate     ]      Applying a function to some values.

FIX how are conditionals defined?
       processor overview?
       change the "description of change in that state" as it changes, depending on the state
       ~ change the function as it evaluates
   currently if is one of nine primitive compiler node types
       the if node translates into hardware-provided conditional branch instructions
           processor overview?

[ Function Arguments ]
        &optional &rest &key &allow-other-keys &aux &body &whole &environment
[ Macros             ]      a type? part of the reader? part of evaluation?
        &body
[ Returning          ]  return return-from values prog1

== FIX ==

[ Print        ]      Printing Lisp.
[ Comments     ]      Annotating program definitions.

[ Binding      ]  Binding symbols to values as variables: let, let*, multiple-value-bind
	lexical,dynamic scope
	extent
        dynamic and static typing
[ Reflection   ]  type-of, typep

== Function ==

lib?  how is primitive comparison defined?
[ Comparison         ]  Comparing values: eq eql equal =
    these kinda go with the type

== Flow of Control ==

stack

all defined in terms of primitives?
[ Blocks          ]  block, progn
[ Conditionals    ]  A choice: cond if fi when fi* case case=
[ Iteration       ]  Looping and recursion: do loop iterate
   loop a pkg
[ Labels          ]  label
[ Local Functions ]  flet
[ Exceptions      ]  catch throw error
[ Conditions      ]  signal handler-bind handler-case

== More ==

defined in terms of primitives?
[ Packages        ]  Grouping functions and variables.
[ Features        ]  lang?   FIX is this (require,provide?
lib
[ Assertions      ]  check-type, assert

[ Summary of Key/All Functions ]
     package browser
     function browser?

== Libraries ==

`Packdired' (initially bound to "control-x p")
[ Packages ]  (from sys usage) describes key packages (~ for programming)

lib?
[ Logical operations ]  or and
[ Bit manipulation   ]  ash log...
[ Doc Nodes ]

List key libs
Mathematics
Arithmetic

== Further Information ==

examples [Directory Layout] tests
]#

#[ Wandering All

    The language is a set of descriptions of state.
    The language can describe a state, including all possible changes
    that the state can undergo.
	; dimension for function vibe, needs to think through handling if
        ; think should focus more on values than objects
    The language can describe the evaluation of objects.  Objects can be
    symbols or structures.  A structure is a grouping of one or more
	; or lists which evaluate as functions, w stack-like arg eval
    optionally named objects.  A symbol is a reference to another object.
    Evaluation of a symbol produces the referred object.  Evaluation of
    a structure produces the structure.

	;; ~ structures and symbols?
    The integers are structures of two symbols, universe and empty.  For
    example 1 is a structure of a structure of a symbol which is universe,
    and 3 is a structure of a structure of two symbols both universe.

        ;; zero dimension
        (defstruct empty)    ; the empty structure, given name empty
        (defstruct universe (:include universe))  ; the universe strct given name universe
        (defstruct)             ; empty?
        (defstruct (:include))  ; universe? the empty structure included in itself


             t is a function that returns itself
             if each slot is thought of as a function
             then structures are ~just collections of functions
                 what about first arg?


        ;; one dimension
        (defstruct fixnum
	  ;; FIX how build in continuity?
	  ;;        prhps integers (combinations) subset of continuity (like generalised below)
	  (0 (type (or empty universe)))
	  (1 (type (or empty universe)))
	  ...
	  (31 (type (or empty universe))))

        (defstruct combination
	  ;; FIX how build in continuity?
	  (0 (type (or empty universe)))
	  (1 (type (or empty universe)))
	  ...
	  (n (type (or empty universe)))) ; FIX how n?
        (defstruct (fixnum (:include (combination 32))))
        (defstruct (integer (:include (combination n?))))
	(defstruct ratio
	  numerator
	  denominator)
	(defstruct generalised
	  (structure :type (or () list))
	  (closure))
        ;; then define ration to include generalised with closure being a / function?

        ;; two dimensions
	(defstruct square
	  (length 0 :type fixnum))     ; Length of edge.
        ;;
        (defstruct user
	  (a (type int))
	  b
	  (c (type (or empty universe)))) ; FIX ?

             how does this relate to functions?
	         functions are a structure?

    Sequences of evaluation can be shared in functions.
]#

#[ Wandering Struct

layout
class
	structure
	built-in

value
   type
       type of value == structure of value
	       type comparison based on structural layout
			   component (field) names?
		   partial equivalence, compatibility
			   somehow every type equiv?
				   equiv to empty type?  (defstruct empty)
       structure of value defined by struct def
		   type of struct def: struct def
	   type equivalence based on components of structure
		   need to name structure defs?
			   to create,include them
				   just a variable holding the structure
					   structure slot on symbol?
	   structural inclusion
		   just a definition mechanism?
		   need every type include a root structure? maybe empty
   a value property, perhaps "class", with a name
       many classes can have the same structure
]#


#[ Lisp Tutorial

== Lists ==

This is a list

    (1 2 3)

A list defines a form.

== Functions ==

Define a function `f'.

    (defun f ()
      3)

The definition, a form, is a list.

Evaluate a call to `f',

    (f)

The call returns 3.  The call, a form, is a list.

Define another function `g', which takes an argument, uses the return from
`f' and has a documentation string.

    (defun g (arg)
      "Return the result of `f' plus the given $arg."
      (+ arg (f)))

When evaluating a function, the name of the function always precedes the
arguments, as in the call to `+' above.

Evaluate a call to `g'

    (g 4)

The function returns 7.

== Variables ==

Define a variable *a* which is initialized to 3.

    (defvar *a* 3)

The definition, a form, is a list.

`Defvar' creates "special" variables, so *a* is special: it has dynamic
binding.  That is, *a* is always in scope.  Naming special with stars like
that is a convention.

Define a function which binds a variable and references *a*.

    (defun h ()
      (let ((b 1))
        (+ *a* b)))

`let' creates lexical variables, so b is lexically bound.  That is, its
scope is limited to the execution of the block defined by the `let' form.
To be clear, during the execution of the `+' function b is out of scope.

Call `h'

    (h)

which returns 4.

== Conditionals ==

This

    ()

is the empty list.

Evaluating the empty list produces a unique value.  This value is used to
represent "false" in conditionals.  Any other value is true.

Define a variable that is false.

   (defvar x ())

Evaluate a conditional form

   (if x 3 4)

which returns 4, because the value of x is false.

== More ==

That's the very basics.  The [Lisp] manual is more thorough.
]#
