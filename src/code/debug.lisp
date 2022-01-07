;;; The inspector.  Includes a basic command-line oriented interface as
;;; well as support for the editor to deliver inspector commands to a
;;; slave.

(in-package "DEBUG")

(export '(internal-debug *in-the-debugger* backtrace *flush-debug-errors*
	  *debug-print-level* *debug-print-length* *debug-prompt*
	  *debug-readtable* *help-line-scroll-count* *stack-top-hint*
	  ;;
	  *auto-eval-in-frame* var arg
	  *only-block-start-locations* *print-location-kind*
	  ;;
	  do-debug-command))

(in-package "LISP")
(export '(invoke-debugger *debugger-hook*))

(in-package "DEBUG")


#[ Debugger

[ Debugger Introduction         ]
[ The Debugger Command Loop     ]
[ Stack Frames                  ]
[ Variable Access               ]
[ Source Location Printing      ]
[ Compiler Policy Control       ]
[ Exiting Commands              ]
[ Information Commands          ]
[ Breakpoint Commands           ]
[ Function Tracing              ]
[ Specials                      ]

[ Debugger Programmer Interface ]
]#

#[ Debugger Introduction

The debugger supports source-level inspection of compiled code.  The
debugger

  * allows access of variables by name,

  * warns when a variable is yet to be initialized or has already been
    deallocated, and

  * can display the precise source location corresponding to a code
    location in the program.

These features allow the debugging of compiled code to be made almost the
same as debugging interpreted code.

The debugger is an interactive command loop that allows a user to examine
the function call stack.  The debugger is invoked when:

  * A \tindexed{serious-condition} is signalled, and it is not handled, or

  * `error' is called, and the condition it signals is not handled, or

  * The debugger is explicitly invoked with the `break' or `debug'
    functions.

On entering the TTY debugger, it looks something like this:

    Error in function CAR.
    Wrong type argument, 3, should have been of type LIST.

    Restarts:
      0: Return to Top-Level.

    Debug  (type H for help)

    (CAR 3)
    0]

The first group of lines describe what the error was that invoked the
debugger.  In this case `car' was called on 3.  After "Restarts:" is a list
of all the ways that we can restart execution after the error.  In this
case, the only option is to return to top-level.  After printing the
banner, the debugger prints the current frame and the debugger prompt.
]#

#[ Stack Frames
\cindex{stack frames} \cpsubindex{frames}{stack}

A stack frame is the run-time representation of a call to a function;
the frame stores the state that a function needs to remember what it is
doing.  Frames have:

  * Variables (\pxlref{debug-vars}), which are the values being operated
    on, and

  * Arguments to the call (which are really just particularly interesting
    variables), and

  * A current location (\pxlref{source-locations}), which is the place in
    the program where the function was running when it stopped to call another
    function, or because of an interrupt or error.

[ Stack Motion                     ]
[ How Arguments are Printed        ]
[ Function Names                   ]
[ Funny Frames                     ]
[ Debug Tail Recursion             ]
[ Unknown Locations and Interrupts ]
]#

#[ Stack Motion

These commands move to a new stack frame and print the name of the function
and the values of its arguments in the style of a Lisp function call:

  % up

    Move up to the next higher frame.  More recent function calls are
    considered to be higher on the stack.

  % down

    Move down to the next lower frame.

  % top

    Move to the highest frame.

  % bottom

    Move to the lowest frame.

  % frame [n]

    Move to the frame with the specified number.  Prompt for the number if
    left out.
]#
#| Hidden from the description above:

  % S function-name n:  Search down the stack for function.  Prompt for the
    function name if left out.  Search an optional number of times, n.

  % R function-name n:  Search up the stack for function.  Prompts for the
    function name if left out.  Search an optional number of times, n.
|#

#[ How Arguments are Printed

A frame is printed to look like a function call, but with the actual
argument values in the argument positions.  So the frame for this call in
the source:

    (myfun (+ 3 4) 'a)

would look like this:

    (MYFUN 7 A)

All keyword and optional arguments are displayed with their actual values;
if the corresponding argument was not supplied, the value will be the
default.  So this call:

    (subseq "foo" 1)

would look like this:

    (SUBSEQ "foo" 1 3)

And this call:

    (string-upcase "test case")

would look like this:

    (STRING-UPCASE "test case" :START 0 :END NIL)


The arguments to a function call are displayed by accessing the argument
variables.  Although those variables are initialized to the actual argument
values, they can be set inside the function; in this case the new value
will be displayed.

\code{&rest} arguments are handled somewhat differently.  The value of
the rest argument variable is displayed as the spread-out arguments to
the call, so:
    (format t "~A is a ~A." "This" 'test)

would look like this:

    (FORMAT T "~A is a ~A." "This" 'TEST)

Rest arguments cause an exception to the normal display of keyword
arguments in functions that have both \code{&rest} and \code{&key}
arguments.  In this case, the keyword argument variables are not displayed
at all; the rest arg is displayed instead.  So for these functions, only
the keywords actually supplied will be shown, and the values displayed will
be the argument values, not values of the (possibly modified) variables.

If the variable for an argument is never referenced by the function, it will be
deleted.  The variable value is then unavailable, so the debugger prints
\code{<unused-arg>} instead of the value.  Similarly, if for any of a number of
reasons (described in more detail in section \ref{debug-vars}) the value of the
variable is unavailable or not known to be available, then
\code{<unavailable-arg>} will be printed instead of the argument value.

Printing of argument values is controlled by *debug-print-level* and
*debug-print-length*.
]#

#[ Function Names
\cpsubindex{function}{names}
\cpsubindex{names}{function}

If a function is defined by `defun', `labels', or `flet', then the debugger
will print the actual function name after the open parenthesis, like:

    (STRING-UPCASE "test case" :START 0 :END NIL)
    ((SETF AREF) #\back a "for" 1)

Otherwise, the function name is a string, and will be printed in quotes:

    ("DEFUN MYFUN" BAR)
    ("DEFMACRO DO" (DO ((I 0 (1+ I))) ((= I 13))) NIL)
    ("SETQ *GC-NOTIFY-BEFORE*")

This string name is derived from the `defmumble' form that encloses or
expanded into the lambda, or the outermost enclosing form if there is no
`defmumble'.
]#

#[ Funny Frames
\cindex{external entry points}
\cpsubindex{entry points}{external}
\cpsubindex{block compilation}{debugger implications}
\cpsubindex{external}{stack frame kind}
\cpsubindex{optional}{stack frame kind}
\cpsubindex{cleanup}{stack frame kind}

Sometimes the evaluator introduces new functions that are used to implement a
user function, but are not directly specified in the source.  The main place
this is done is for checking argument type and syntax.  Usually these functions
do their thing and then go away, and thus are not seen on the stack in the
debugger.  But when you get some sort of error during lambda-list processing,
you end up in the debugger on one of these funny frames.

These funny frames are flagged by printing "[:keyword]" after the
parentheses.  For example, this call:

    (car 'a 'b)

will look like this:

    (CAR 2 A) [:EXTERNAL]

And this call:

    (string-upcase "test case" :end)

would look like this:

    ("DEFUN STRING-UPCASE" "test case" 335544424 1) [:OPTIONAL]


As you can see, these frames have only a vague resemblance to the original
call.  Fortunately, the error message displayed when you enter the debugger
will usually tell you what problem is (in these cases, too many arguments
and odd keyword arguments.)  Also, if you go down the stack to the frame
for the calling function, you can display the original source
(\pxlref{source-locations}.)

With recursive or block compiled functions (\pxlref{block-compilation}), an
:EXTERNAL frame may appear before the frame representing the first call to
the recursive function or entry to the compiled block.  This is a
consequence of the way the compiler does block compilation: there is
nothing odd with your program.  You will also see :CLEANUP frames during
the execution of \code{unwind-protect} cleanup code.  Note that inline
expansion and open-coding affect what frames are present in the debugger,
see sections \ref{debugger-policy} and \ref{open-coding}.
]#

#[ Debug Tail Recursion
\cindex{tail recursion}
\cpsubindex{recursion}{tail}

Both the compiler and the interpreter are "properly tail recursive."  If a
function call is in a tail-recursive position, the stack frame will be
deallocated \i{at the time of the call}, rather than after the call
returns.
Consider this backtrace:
    (BAR ...)
    (FOO ...)

Because of tail recursion, it is not necessarily the case that `FOO'
directly called `BAR'.  It may be that `FOO' called some other function
`FOO2' which then called `BAR' tail-recursively, as in this example:

    (defun foo ()
      ...
      (foo2 ...)
      ...)

    (defun foo2 (...)
      ...
      (bar ...))

    (defun bar (...)
      ...)


Usually the elimination of tail-recursive frames makes debugging more
pleasant, since these frames are mostly uninformative.  If there is any
doubt about how one function called another, it can usually be eliminated
by finding the source location in the calling frame (section
\ref{source-locations}.)

For a more thorough discussion of tail recursion, \pxlref{tail-recursion}.
]#

#[ Unknown Locations and Interrupts
\cindex{unknown code locations}
\cpsubindex{locations}{unknown}
\cindex{interrupts}
\cpsubindex{errors}{run-time}

The debugger operates using special debugging information attached to the
compiled code.  This debug information tells the debugger what it needs to
know about the locations in the code where the debugger can be invoked.  If
the debugger somehow encounters a location not described in the debug
information, then it is said to be \var{unknown}.  If the code location for
a frame is unknown, then some variables may be inaccessible, and the source
location cannot be precisely displayed.

There are three reasons why a code location could be unknown:

  * There is inadequate debug information due to the value of the
    \code{debug} optimization quality.  \xlref{debugger-policy}.

  * The debugger was entered because of an interrupt such as \code{^C}.

  * A hardware error such as "\code{bus error}" occurred in code that was
    compiled unsafely due to the value of the \code{safety} optimization
    quality.  \xlref{optimize-declaration}.

In the last two cases, the values of argument variables are accessible, but
may be incorrect.  \xlref{debug-var-validity} for more details on when
variable values are accessible.

It is possible for an interrupt to happen when a function call or return is
in progress.  The debugger may then flame out with some obscure error or
insist that the bottom of the stack has been reached, when the real problem
is that the current stack frame can't be located.  If this happens, return
from the interrupt and try again.

When running interpreted code, all locations should be known.  However, an
interrupt might catch some subfunction of the interpreter at an unknown
location.  In this case, you should be able to go up the stack a frame or
two and reach an interpreted frame which can be debugged.
]#

#[ Variable Access
\cpsubindex{variables}{debugger access}
\cindex{debug variables}

There are three ways to access the current frame's local variables in the
debugger.  The simplest is to type the variable's name into the debugger's
read-eval-print loop.  The debugger will evaluate the variable reference as
though it had appeared inside that frame.

The debugger doesn't really understand lexical scoping; it has just one
namespace for all the variables in a function.  If a symbol is the name of
multiple variables in the same function, then the reference appears
ambiguous, even though lexical scoping specifies which value is visible at
any given source location.  If the scopes of the two variables are not
nested, then the debugger can resolve the ambiguity by observing that only
one variable is accessible.

When there are ambiguous variables, the evaluator assigns each one a small
integer identifier.  The `debug:var' function and the "list-locals" command
use this identifier to distinguish between ambiguous variables:

  % list-locals prefix*

    Print the name and value of all variables in the current frame whose
    name has the specified $prefix.  $prefix may be a string or a symbol.
    If no $prefix is given, then all available variables are printed.  If a
    variable has a potentially ambiguous name, then the name is printed
    with a "#identifier" suffix, where identifier is the small integer used
    to make the name unique.

{function:debug:var}

[ Variable Value Availability     ]
[ Note On Lexical Variable Access ]
]#

#[ Variable Value Availability
\cindex{availability of debug variables}
\cindex{validity of debug variables}
\cindex{debug optimization quality}

The value of a variable may be unavailable to the debugger in portions of
the program where Lisp says that the variable is defined.  If a variable
value is not available, the debugger will not let you read or write that
variable.  With one exception, the debugger will never display an incorrect
value for a variable.  Rather than displaying incorrect values, the
debugger tells you the value is unavailable.

The one exception is this: if you interrupt (e.g., with "control-c") or if
there is an unexpected hardware error such as "bus error" (which should
only happen in unsafe code), then the values displayed for arguments to the
interrupted frame might be incorrect.  (Since the location of an interrupt
or hardware error will always be an unknown location
(\pxlref{unknown-locations}), non-argument variable values will never be
available in the interrupted frame.)  This exception applies only to the
interrupted frame: any frame farther down the stack will be fine.

The value of a variable may be unavailable for these reasons:

  * The value of the \code{debug} optimization quality may have omitted debug
    information needed to determine whether the variable is available.
    Unless a variable is an argument, its value will only be available when
    \code{debug} is at least 2.

  * The compiler did lifetime analysis and determined that the value was no
    longer needed, even though its scope had not been exited.  Lifetime
    analysis is inhibited when the \code{debug} optimization quality is
    3.

  * The variable's name is an uninterned symbol (gensym).  To save space,
    the compiler only dumps debug information about uninterned variables
    when the \code{debug} optimization quality is 3.

  * The frame's location is unknown (\pxlref{unknown-locations}) because the
    debugger was entered due to an interrupt or unexpected hardware error.
    Under these conditions the values of arguments will be available, but
    might be incorrect.  This is the exception above.

  * The variable was optimized out of existence.  Variables with no reads are
    always optimized away, even in the interpreter.  The degree to which the
    compiler deletes variables will depend on the value of the
    \code{compile-speed} optimization quality, but most source-level
    optimizations are done under all compilation policies.

Since it is especially useful to be able to get the arguments to a
function, argument variables are treated specially when the \code{speed}
optimization quality is less than 3 and the \code{debug} quality is at
least 1.  With this compilation policy, the values of argument variables
are almost always available everywhere in the function, even at unknown
locations.  For non-argument variables, \code{debug} must be at least 2 for
values to be available, and even then, values are only available at known
locations.
]#

#[ Note On Lexical Variable Access
\cpsubindex{evaluation}{debugger}

When the debugger command loop establishes variable bindings for available
variables, these variable bindings have lexical scope and dynamic extent.
(The variable bindings are actually created using `symbol-macro-let'
special form.)  You can close over them, but such closures can't be used as
upward funargs.

You can also set local variables using `setq', but if the variable was
closed over in the original source and never set, then setting the variable
in the debugger may not change the value in all the functions the variable
is defined in.  Another risk of setting variables is that you may assign a
value of a type that the compiler proved the variable could never take on.
This may result in bad things happening.
]#

#[ Source Location Printing
\cpsubindex{source location printing}{debugger}

Source level debugging of compiled code is available.  These commands
display the source location for the current frame:

  % source context*

    Display the file that the current frame's function was defined from (if
    it was defined from a file), and then the source form responsible for
    generating the code that the current frame was executing.  If context
    is specified, then it is an integer specifying the number of enclosing
    levels of list structure to print.

  % vsource context*

    This command is identical to "source", except that it uses the global
    values of *print-level* and *print-length* instead of the debugger
    printing control variables *debug-print-level* and
    *debug-print-length*.

The source form for a location in the code is the innermost list present in
the original source that encloses the form responsible for generating that
code.  If the actual source form is not a list, then some enclosing list
will be printed.  For example, if the source form was a reference to the
variable *some-random-special*, then the innermost enclosing evaluated form
will be printed.  Here are some possible enclosing forms:

    (let ((a *some-random-special*))
      ...)

    (+ *some-random-special* ...)


If the code at a location was generated from the expansion of a macro or a
source-level compiler optimization, then the form in the original source
that expanded into that code will be printed.  Suppose the file
"/home/ed/stuff.lisp" looked like this:

    (defmacro mymac ()
      '(myfun))

    (defun foo ()
      (mymac)
      ...)

If `foo' has called `myfun', and is waiting for it to return, then the
"source" command would print:

    ; File: /home/ed/stuff.lisp

    (MYMAC)

Note that the macro use was printed, not the actual function call form,
\code{(myfun)}.

If enclosing source is printed by giving an argument to \code{source} or
\code{vsource}, then the actual source form is marked by wrapping it in a
list whose first element is "#:***HERE***".  In the previous example,
\code{source 1} would print:

    ; File: /home/ed/stuff.lisp

    (DEFUN FOO ()
      (#:***HERE***
       (MYMAC))
      ...)

[ How the Source is Found      ]
[ Source Location Availability ]
]#

#[ How the Source is Found

If the code was defined from Lisp by `compile' or `eval', then the source
can always be reliably located.  If the code was defined from a FASL file
created by `compile-file', then the debugger gets the source forms it
prints by reading them from the original source file.  This is a potential
problem, since the source file might have moved or changed since the time
it was compiled.

The source file is opened using the `truename' of the source file pathname
originally given to the compiler.  This is an absolute pathname with all
logical names and symbolic links expanded.  If the file can't be located
using this name, then the debugger gives up and signals an error.

If the source file can be found, but has been modified since the time it was
compiled, the debugger prints this warning:

    ; File has been modified since compilation:
    ;   "filename
    ; Using form offset instead of character position.

where "filename" is the name of the source file.  It then proceeds using a
robust but not foolproof heuristic for locating the source.  This heuristic
works if:

  * No top-level forms before the top-level form containing the source have
    been added or deleted, and

  * The top-level form containing the source has not been modified much.
    (More precisely, none of the list forms beginning before the source form
    have been added or deleted.)

If the heuristic doesn't work, the displayed source will be wrong, but will
probably be near the actual source.  If the "shape" of the top-level form in
the source file is too different from the original form, then an error will be
signalled.  When the heuristic is used, the the source location commands are
noticeably slowed.

Source location printing can also be confused if (after the source was
compiled) a read-macro you used in the code was redefined to expand into
something different, or if a read-macro ever returns the same \code{eq}
list twice.  If you don't define read macros and don't use \code{##} in
perverted ways, you don't need to worry about this.
]#

#[ Source Location Availability

\cindex{debug optimization quality}
Source location information is only available when the \code{debug}
optimization quality is at least 2.  If source location information is
unavailable, the source commands will give an error message.

If source location information is available, but the source location is
unknown because of an interrupt or unexpected hardware error
(\pxlref{unknown-locations}), then the command will print:
    Unknown location: using block start.

and then proceed to print the source location for the start of the \i{basic
block} enclosing the code location. \cpsubindex{block}{basic}
\cpsubindex{block}{start location}
It's a bit complicated to explain exactly what a basic block is, but
here are some properties of the block start location:

  * The block start location may be the same as the true location.

  * The block start location will never be later in the the program's flow
    of control than the true location.

  * No conditional control structures (such as `if', `cond', `or') will
    intervene between the block start and the true location (but note that
    some conditionals present in the original source could be optimized
    away.)  Function calls \i{do not} end basic blocks.

  * The head of a loop will be the start of a block.

  * The programming language concept of "block structure" and the `block'
    special form are totally unrelated to the compiler's basic block.

In other words, the true location lies between the printed location and the
next conditional (but watch out because the compiler may have changed the
program).
]#

#[ Compiler Policy Control
\cpsubindex{policy}{debugger}
\cindex{debug optimization quality}
\cindex{optimize declaration}

The compilation policy specified by \code{optimize} declarations affects
the behavior seen in the debugger.  The \code{debug} quality directly
affects the debugger by controlling the amount of debugger information
dumped.  Other optimization qualities have indirect but observable effects
due to changes in the way compilation is done.

Unlike the other optimization qualities (which are compared in relative value
to evaluate tradeoffs), the \code{debug} optimization quality is directly
translated to a level of debug information.  This absolute interpretation
allows the user to count on a particular amount of debug information being
available even when the values of the other qualities are changed during
compilation.  These are the levels of debug information that correspond to the
values of the \code{debug} quality:

  % 0

    Only the function name and enough information to allow the stack to be
    parsed.

  % > 0

    Any level greater than \code{0} gives level \code{0} plus all argument
    variables.  Values will only be accessible if the argument variable is
    never set and \code{speed} is not 3.  Nightshade allows any real value
    for optimization qualities.  It may be useful to specify 0.5 to get
    backtrace argument display without argument documentation.

  % 1

    Level 1 provides argument documentation (printed arglists) and derived
    argument/result type information.  This makes \findexed{describe} more
    informative, and allows the compiler to do compile-time argument count
    and type checking for any calls compiled at run-time.

  % 2

    Level 1 plus all interned local variables, source location information,
    and lifetime information that tells the debugger when arguments are
    available (even when \code{speed} is 3 or the argument is set.)  This
    is the default.

  % 3

    Level 2 plus all uninterned variables.  In addition, lifetime analysis
    is disabled (even when \code{speed} is 3), ensuring that all variable
    values are available at any known location within the scope of the
    binding.  This has a speed penalty in addition to the obvious space
    penalty.

As you can see, if the \code{speed} quality is 3, debugger performance is
degraded.  This effect comes from the elimination of argument variable
special-casing (\pxlref{debug-var-validity}.)  Some degree of
speed/debuggability tradeoff is required, but the effect is not too drastic
when \code{debug} is at least 2.

\cindex{inline expansion}
\cindex{semi-inline expansion}

In addition to \code{inline} and \code{notinline} declarations, the
relative values of the \code{speed} and \code{space} qualities also change
whether functions are inline expanded (\pxlref{inline-expansion}.)  If a
function is inline expanded, then there will be no frame to represent the
call, and the arguments will be treated like any other local variable.
Functions may also be "semi-inline", in which case there is a frame to
represent the call, but the call is to an optimized local version of the
function, not to the original function.
]#


#[ Function Tracing
\cindex{tracing}
\cpsubindex{function}{tracing}

The tracer causes selected functions to print their arguments and their
results whenever they are called.  Options allow conditional printing of
the trace information and conditional breakpoints on function entry or
exit.

{function:trace}
{function:untrace}

{variable:ext:*max-trace-indentation*}

[ Encapsulation Functions ]
]#

#[ Encapsulation Functions
\cindex{encapsulation}
\cindex{advising}

The encapsulation functions provide a mechanism for intercepting the
arguments and results of a function.  `encapsulate' changes the function
definition of a symbol, and saves it so that it can be restored later.  The
new definition normally calls the original definition.  The function
`fdefinition' always returns the original definition, stripping off any
encapsulation.

The original definition of the symbol can be restored at any time by the
`unencapsulate' function.  `encapsulate' and `unencapsulate' allow a symbol
to be multiply encapsulated in such a way that different encapsulations can
be completely transparent to each other.

Each encapsulation has a type which may be an arbitrary lisp object.  If a
symbol has several encapsulations of different types, then any one of them
can be removed without affecting more recent ones.  A symbol may have more
than one encapsulation of the same type, but only the most recent one can
be undone.

{function:ext:encapsulate}
{function:ext:unencapsulate}
{function:ext:encapsulated-p}
]#

;; FIX was this a planned feature?
#[ The Single Stepper

\defmac{step}{ \args{\var{form}}}
Evaluates form with single stepping enabled or if \var{form} is \code{T},
enables stepping until explicitly disabled.  Stepping can be
disabled by quitting to the lisp top level, or by evaluating the form
\w{\code{(step ())}}.

While stepping is enabled, every call to eval will prompt the user for
a single character command.  The prompt is the form which is about to
be \code{eval}ed.  It is printed with \code{*print-level*} and
\code{*print-length*} bound to \code{*step-print-level*} and
\code{*step-print-length*}.  All interaction is done through the stream
\code{*query-io*}.  Because of this, the stepper can not be used in the editor
eval mode.  When connected to a slave Lisp, the stepper can be used
from the editor.

The commands are:
\begin{description}

\item[\key{n} (next)]
Evaluate the expression with stepping still enabled.

\item[\key{s} (skip)]
Evaluate the expression with stepping disabled.

\item[\key{q} (quit)]
Evaluate the expression, but disable all further
stepping inside the current call to \code{step}.

\item[\key{p} (print)]
Print current form.  (does not use
\code{*step-print-level*} or \code{*step-print-length*}.)

\item[\key{b} (break)]
Enter break loop, and then prompt for the command
again when the break loop returns.

\item[\key{e} (eval)]
Prompt for and evaluate an arbitrary expression.
The expression is evaluated with stepping disabled.

\item[\key{?} (help)]
Prints a brief list of the commands.

\item[\key{r} (return)]
Prompt for an arbitrary value to return as result
of the current call to eval.

\item[\key{g}]
Throw to top level.
\end{description}
\enddefmac

\defvar{step-print-level}[extensions]
\defvarx{step-print-length}[extensions]
\code{*print-level*} and \code{*print-length*} are bound to these values while
printing the current form.  \code{*Step-print-level*} and
\code{*step-print-length*} are initially bound to 4 and 5, respectively.
\enddefvar

\defvar{max-step-indentation}[extensions]
Step indents the prompts to highlight the nesting of the evaluation.
This variable contains the maximum number of spaces to use for
indenting.  Initially set to 40.
\enddefvar
]#


;;; Used to communicate to debug-loop that we are at a step breakpoint.
;;;
(define-condition step-condition (simple-condition) ())


;;;; Variables, parameters, and constants.

#[ Specials

These are the special variables that control the debugger action.

{variable:debug:*debug-print-level*}
{variable:debug:*debug-print-length*}

When evaluating arbitrary expressions in the debugger, the normal values of
*print-level* and *print-length* are in effect.
]#

(defparameter *debug-print-level* 3
  "*print-level* is bound to this value when debug prints a function call.
   If (), use *print-level*")

(defparameter *debug-print-length* 5
  "*print-length* is bound to this value when debug prints a function call.
   If null, use *print-length*.")

(defvar *in-the-debugger* nil
  "This is #t while in the debugger.")

(defvar *debug-command-level* 0
  "Pushes and pops/exits inside the debugger change this.")

(defvar *stack-top-hint* nil
  "If this is bound before the debugger is invoked, it is used as the stack
   top by the debugger.")
(defvar *stack-top* nil)
(defvar *real-stack-top* nil)

(defvar *current-frame* nil)

;;; DEBUG-PROMPT -- Internal.
;;;
;;; This is the default for *debug-prompt*.
;;;
(defun debug-prompt ()
  (let ((*standard-output* *debug-io*))
    (terpri)
    (prin1 (di:frame-number *current-frame*))
    (dotimes (i *debug-command-level*) (princ "]"))
    (princ " ")
    (force-output)))

(defparameter *debug-prompt* #'debug-prompt
  "This is a function of no arguments that prints the debugger prompt
   on *debug-io*.")

(defconstant debug-help-string
"
The prompt is the right square brackets; the number of brackets indicates how many
  recursive command loops are running.
Debug commands do not affect * and friends, but evaluation in the debug loop
  do affect these variables.
Any command may be uniquely abbreviated.

Getting in and out of DEBUG:
  Q        throws to top level.
  GO       calls CONTINUE which tries to proceed with the restart 'continue.
  RESTART  invokes restart numbered as shown (prompt if not given).
  ERROR    prints the error condition and restart cases.
  FLUSH    toggles *flush-debug-errors*, which is initially t.

  The name of any restart, or its number, is a valid command, and is the same
    as using RESTART to invoke that restart.

Changing frames:
  U  up frame        D  down frame       T  top frame       B  bottom frame

  F n   goes to frame n.

Inspecting frames:
  BACKTRACE [n]   shows n frames going down the stack.
  L               lists locals in current function.
  P, PP           displays current function call.
  SOURCE [n]      displays frame's source form with n levels of enclosing forms.
  VSOURCE [n]     displays frame's source form without any ellipsis.
  DESCRIBE        describe current function call.

Inspecting values:
  INSPECT [var]  inspect the contents of variable var.

Breakpoints and steps:
  LIST-LOCATIONS [{function | :c}]  list the locations for breakpoints.
    Specify :c for the current frame.  Abbreviation: LL
  LIST-BREAKPOINTS                  list the active breakpoints.
    Abbreviations: LB, LBP
  DELETE-BREAKPOINT [n]             remove breakpoint n or all breakpoints.
    Abbreviations: DEL, DBP
  BREAKPOINT {n | :end | :start} [:break form] [:function function]
    [{:print form}*] [:condition form]    set a breakpoint.
    Abbreviations: BR, BP
  STEP [n]                          step to the next location or step n times.

Editor commands (in a slave):
  EDIT-SOURCE  edit the source of the current function call.
  ED-HELP      read about the Debugger in the editor documentation system.

Function and macro commands:
  (DEBUG:DEBUG-RETURN expression)
    returns expression's values from the current frame, exiting the debugger.
  (DEBUG:ARG n)
    returns the n'th argument, remaining in the debugger.
  (DEBUG:VAR string-or-symbol [id])
    returns the specified variable's value, remaining in the debugger.
")


;;;; Breakpoint state.

(defvar *only-block-start-locations* nil
  "When true, the LIST-LOCATIONS command only displays block start locations.
   Otherwise, all locations are displayed.")

(defvar *print-location-kind* nil
  "If true, list the code location type in the LIST-LOCATIONS command.")

;;; A list of the types of code-locations that should not be stepped to and
;;; should not be listed when listing breakpoints.
;;;
(defvar *bad-code-location-types* '(:call-site :internal-error))
(declaim (type list *bad-code-location-types*))

;;; Code locations of the possible breakpoints
;;;
(defvar *possible-breakpoints*)
(declaim (type list *possible-breakpoints*))

;;; A list of the made and active breakpoints, each is a breakpoint-info
;;; structure.
;;;
(defvar *breakpoints* nil)
(declaim (type list *breakpoints*))

;;; A list of breakpoint-info structures of the made and active step
;;; breakpoints.
;;;
(defvar *step-breakpoints* nil)
(declaim (type list *step-breakpoints*))

;;; Number of times left to step.
;;;
(defvar *number-of-steps* 1)
(declaim (type integer *number-of-steps*))

;;; Used when listing and setting breakpoints.
;;;
(defvar *default-breakpoint-debug-function* nil)
(declaim (type (or list di:debug-function) *default-breakpoint-debug-function*))


;;;; Code location utilities.

;;; FIRST-CODE-LOCATION -- Internal.
;;;
;;; Returns the first code-location in the passed debug block
;;;
(defun first-code-location (debug-block)
  (let ((found nil)
	(first-code-location nil))
    (di:do-debug-block-locations (code-location debug-block)
      (unless found
	(setf first-code-location code-location)
	(setf found t)))
    first-code-location))

;;; NEXT-CODE-LOCATIONS -- Internal.
;;;
;;; Returns a list of the next code-locations following the one passed.  One of
;;; the *bad-code-location-types* will not be returned.
;;;
(defun next-code-locations (code-location)
  (let ((debug-block (di:code-location-debug-block code-location))
	(block-code-locations nil))
    (di:do-debug-block-locations (block-code-location debug-block)
      (unless (member (di:code-location-kind block-code-location)
		      *bad-code-location-types*)
	(push block-code-location block-code-locations)))
    (setf block-code-locations (nreverse block-code-locations))
    (let* ((code-loc-list (rest (member code-location block-code-locations
					:test #'di:code-location=)))
	   (next-list (cond (code-loc-list
			     (list (first code-loc-list)))
			    ((map 'list #'first-code-location
				  (di:debug-block-successors debug-block)))
			    (t nil))))
      (when (and (= (length next-list) 1)
		 (di:code-location= (first next-list) code-location))
	(setf next-list (next-code-locations (first next-list))))
      next-list)))

;;; POSSIBLE-BREAKPOINTS -- Internal.
;;;
;;; Returns a list of code-locations of the possible breakpoints of the
;;; debug-function passed.
;;;
(defun possible-breakpoints (debug-function)
  (let ((possible-breakpoints nil))
    (di:do-debug-function-blocks (debug-block debug-function)
      (unless (di:debug-block-elsewhere-p debug-block)
	(if *only-block-start-locations*
	    (push (first-code-location debug-block) possible-breakpoints)
	    (di:do-debug-block-locations (code-location debug-block)
	      (when (not (member (di:code-location-kind code-location)
				 *bad-code-location-types*))
		(push code-location possible-breakpoints))))))
    (nreverse possible-breakpoints)))

;;; LOCATION-IN-LIST -- Internal.
;;;
;;; Searches the info-list for the item passed (code-location, debug-function,
;;; or breakpoint-info).  If the item passed is a debug function then kind will
;;; be compared if it was specified.  The kind if also compared if a
;;; breakpoint-info is passed since it's in the breakpoint.  The info structure
;;; is returned if found.
;;;
(defun location-in-list (place info-list &optional (kind nil))
  (when (breakpoint-info-p place)
    (setf kind (di:breakpoint-kind (breakpoint-info-breakpoint place)))
    (setf place (breakpoint-info-place place)))
  (cond ((di:code-location-p place)
	 (find place info-list
	       :key #'breakpoint-info-place
	       :test #'(lambda (x y) (and (di:code-location-p y)
					  (di:code-location= x y)))))
	(t
	 (find place info-list
	       :test #'(lambda (x-debug-function y-info)
			 (let ((y-place (breakpoint-info-place y-info))
			       (y-breakpoint (breakpoint-info-breakpoint
					      y-info)))
			   (and (di:debug-function-p y-place)
				(eq x-debug-function y-place)
				(or (not kind)
				    (eq kind (di:breakpoint-kind
					      y-breakpoint))))))))))

;;; MAYBE-BLOCK-START-LOCATION  --  Internal.
;;;
;;; If Loc is an unknown location, then try to find the block start location.
;;; Used by source printing to some information instead of none for the user.
;;;
(defun maybe-block-start-location (loc)
  (if (di:code-location-unknown-p loc)
      (let* ((block (di:code-location-debug-block loc))
	     (start (di:do-debug-block-locations (loc block)
		      (return loc))))
	(cond ((and (not (di:debug-block-elsewhere-p block))
		    start)
	       (format t "~%Unknown location: using block start.~%")
	       start)
	      (t
	       loc)))
      loc))


;;;; The BREAKPOINT-INFO structure.

;;; Hold info about made breakpoints
;;;
(defstruct breakpoint-info
  ;;
  ;; Where we are going to stop.
  (place (required-argument) :type (or di:code-location di:debug-function))
  ;;
  ;; The breakpoint returned by di:make-breakpoint.
  (breakpoint (required-argument) :type di:breakpoint)
  ;;
  ;; Function returned from di:preprocess-for-eval.  If result is true,
  ;; drop into the debugger.
  (break #'identity :type function)
  ;;
  ;; Function returned from di:preprocess-for-eval.  If result is true,
  ;; eval (each) print and print results.
  (condition #'identity :type function)
  ;;
  ;; List of functions from di:preprocess-for-eval to evaluate, results are
  ;; conditionally printed.  Car of each element is the function, cdr is the
  ;; form it goes with.
  (print nil :type list)
  ;;
  ;; The number used when listing the possible breakpoints within a function.
  ;; Could also be a symbol such as start or end.
  (code-location-number (required-argument) :type (or symbol integer))
  ;;
  ;; The number used when listing the breakpoints active and to delete
  ;; breakpoints.
  (breakpoint-number (required-argument) :type integer))

;;; CREATE-BREAKPOINT-INFO -- Internal.
;;;
;;; Returns a new breakpoint-info structure with the info passed.
;;;
(defun create-breakpoint-info (place breakpoint code-location-number
				     &key (break #'identity)
				     (condition #'identity) (print nil))
  (setf *breakpoints*
	(sort *breakpoints* #'< :key #'breakpoint-info-breakpoint-number))
  (let ((breakpoint-number
	 (do ((i 1 (incf i)) (breakpoints *breakpoints* (rest breakpoints)))
	     ((or (> i (length *breakpoints*))
		  (not (= i (breakpoint-info-breakpoint-number
			     (first breakpoints)))))

	      i))))
    (make-breakpoint-info :place place :breakpoint breakpoint
			  :code-location-number code-location-number
			  :breakpoint-number breakpoint-number
			  :break break :condition condition :print print)))

;;; PRINT-BREAKPOINT-INFO -- Internal.
;;;
;;; Prints the breakpoint info for the breakpoint-info structure passed.
;;;
(defun print-breakpoint-info (breakpoint-info)
  (let ((place (breakpoint-info-place breakpoint-info))
	(bp-number (breakpoint-info-breakpoint-number breakpoint-info))
	(loc-number (breakpoint-info-code-location-number breakpoint-info)))
    (case (di:breakpoint-kind (breakpoint-info-breakpoint breakpoint-info))
      (:code-location
       (print-code-location-source-form place 0)
       (format t "~&~S: ~S in ~S"
	       bp-number loc-number (di:debug-function-name
				      (di:code-location-debug-function place))))
      (:function-start
       (format t "~&~S: FUNCTION-START in ~S" bp-number
	       (di:debug-function-name place)))
      (:function-end
       (format t "~&~S: FUNCTION-END in ~S" bp-number
	       (di:debug-function-name place))))))


;;;; Main-hook-function for steps and breakpoints.

;;; MAIN-HOOK-FUNCTION -- Internal.
;;;
;;; Must be passed as the hook function.  Keeps track of where step
;;; breakpoints are.
;;;
(defun main-hook-function (current-frame breakpoint &optional return-vals
					 function-end-cookie)
  (setf *default-breakpoint-debug-function*
	(di:frame-debug-function current-frame))
  (dolist (step-info *step-breakpoints*)
    (di:delete-breakpoint (breakpoint-info-breakpoint step-info))
    (let ((bp-info (location-in-list step-info *breakpoints*)))
      (when bp-info
	(di:activate-breakpoint (breakpoint-info-breakpoint bp-info)))))
  (let ((*stack-top-hint* current-frame)
	(step-hit-info
	 (location-in-list (di:breakpoint-what breakpoint)
			   *step-breakpoints* (di:breakpoint-kind breakpoint)))
	(bp-hit-info
	 (location-in-list (di:breakpoint-what breakpoint)
			   *breakpoints* (di:breakpoint-kind breakpoint)))
	(break)
	(condition)
	(string ""))
    (setf *step-breakpoints* nil)
    (labels ((build-string (str)
	       (setf string (concatenate 'string string str)))
	     (print-common-info ()
	       (build-string
		(with-output-to-string (*standard-output*)
		  (when function-end-cookie
		    (format t "~%Return values: ~S" return-vals))
		  (when condition
		    (when (breakpoint-info-print bp-hit-info)
		      (format t "~%")
		      (print-frame-call current-frame))
		    (dolist (print (breakpoint-info-print bp-hit-info))
		      (format t "~& ~S = ~S" (rest print)
			      (funcall (first print) current-frame))))))))
      (when bp-hit-info
	(setf break (funcall (breakpoint-info-break bp-hit-info)
			     current-frame))
	(setf condition (funcall (breakpoint-info-condition bp-hit-info)
				 current-frame)))
      (cond ((and bp-hit-info step-hit-info (= 1 *number-of-steps*))
	     (build-string (format nil "~&*Step (to a breakpoint)*"))
	     (print-common-info)
	     (break string))
	    ((and bp-hit-info step-hit-info break)
	     (build-string (format nil "~&*Step (to a breakpoint)*"))
	     (print-common-info)
	     (break string))
	    ((and bp-hit-info step-hit-info)
	     (print-common-info)
	     (format t "~A" string)
	     (decf *number-of-steps*)
	     (set-step-breakpoint current-frame))
	    ((and step-hit-info (= 1 *number-of-steps*))
	     (build-string "*Step*")
	     (break (make-condition 'step-condition :format-control string)))
	    (step-hit-info
	     (decf *number-of-steps*)
	     (set-step-breakpoint current-frame))
	    (bp-hit-info
	     (when break
	       (build-string (format nil "~&*Breakpoint hit*")))
	     (print-common-info)
	     (if break
		 (break string)
		 (format t "~A" string)))
	    (t
	     (break "Error in main-hook-function: unknown breakpoint"))))))


;;; SET-STEP-BREAKPOINT -- Internal.
;;;
;;; Sets breakpoints at the next possible code-locations.  After calling
;;; this either (continue) if in the debugger or just let program flow
;;; return if in a hook function.
;;;
(defun set-step-breakpoint (frame)
  (cond
   ((di:debug-block-elsewhere-p (di:code-location-debug-block
				 (di:frame-code-location frame)))
    (format t "Cannot step, in elsewhere code~%"))
   (t
    (let* ((code-location (di:frame-code-location frame))
	   (next-code-locations (next-code-locations code-location)))
      (cond
       (next-code-locations
	(dolist (code-location next-code-locations)
	  (let ((bp-info (location-in-list code-location *breakpoints*)))
	    (when bp-info
	      (di:deactivate-breakpoint (breakpoint-info-breakpoint bp-info))))
	  (let ((bp (di:make-breakpoint #'main-hook-function code-location
					:kind :code-location)))
	    (di:activate-breakpoint bp)
	    (push (create-breakpoint-info code-location bp 0)
		  *step-breakpoints*))))
       (t
	(let* ((debug-function (di:frame-debug-function *current-frame*))
	       (bp (di:make-breakpoint #'main-hook-function debug-function
				       :kind :function-end)))
	  (di:activate-breakpoint bp)
	  (push (create-breakpoint-info debug-function bp 0)
		*step-breakpoints*))))))))


;;; BACKTRACE -- Public.
;;;
(defun backtrace (&optional (count most-positive-fixnum)
			    (*standard-output* *debug-io*))
  "Show a listing of the call stack going down from the current frame.  In
   the debugger, the current frame is indicated by the prompt.  Count is
   how many frames to show."
  (let ((*print-length* (or *debug-print-length* *print-length*))
	(*print-level* (or *debug-print-level* *print-level*)))
    (fresh-line *standard-output*)
    (do ((frame (if *in-the-debugger* *current-frame* (di:top-frame))
		(di:frame-down frame))
	 (count count (1- count)))
	((or (null frame) (zerop count)))
      (print-frame-call frame :number t))
    (fresh-line *standard-output*)
    (values)))


;;;; Frame printing.

(eval-when (compile eval)

;;; LAMBDA-LIST-ELEMENT-DISPATCH -- Internal.
;;;
;;; This is a convenient way to express what to do for each type of lambda-list
;;; element.
;;;
(defmacro lambda-list-element-dispatch (element &key required optional rest
						keyword deleted)
  `(etypecase ,element
     (di:debug-variable
      ,@required)
     (cons
      (ecase (car ,element)
	(:optional ,@optional)
	(:rest ,@rest)
	(:keyword ,@keyword)))
     (symbol
      (assert (eq ,element :deleted))
      ,@deleted)))

(defmacro lambda-var-dispatch (variable location deleted valid other)
  (let ((var (gensym)))
    `(let ((,var ,variable))
       (cond ((eq ,var :deleted) ,deleted)
	     ((eq (di:debug-variable-validity ,var ,location) :valid) ,valid)
	     (t ,other)))))

) ;EVAL-WHEN

;;; This is used in constructing arg lists for debugger printing when the arg
;;; list is unavailable, some arg is unavailable or unused, etc.
;;;
(defstruct (unprintable-object
	    (:constructor make-unprintable-object (string))
	    (:print-function (lambda (x s d)
			       (declare (ignore d))
			       (format s "#<~A>"
				       (unprintable-object-string x)))))
  string)

;;; PRINT-FRAME-CALL-1 -- Internal.
;;;
;;; This prints frame with verbosity level 1.  If we hit a rest-arg, then
;;; print as many of the values as possible, punting the loop over
;;; lambda-list variables since any other arguments will be in the
;;; rest-arg's list of values.
;;;
(defun print-frame-call-1 (frame)
  (let* ((d-fun (di:frame-debug-function frame))
	 (loc (di:frame-code-location frame))
	 (results (list (di:debug-function-name d-fun))))
    (handler-case
	(dolist (ele (di:debug-function-lambda-list d-fun))
	  (lambda-list-element-dispatch ele
	    :required ((push (frame-call-arg ele loc frame) results))
	    :optional ((push (frame-call-arg (second ele) loc frame) results))
	    :keyword ((push (second ele) results)
		      (push (frame-call-arg (third ele) loc frame) results))
	    :deleted ((push (frame-call-arg ele loc frame) results))
	    :rest ((lambda-var-dispatch (second ele) loc
		     nil
		     (progn
		       (setf results
			     (append (reverse (di:debug-variable-value
					       (second ele) frame))
				     results))
		       (return))
		     (push (make-unprintable-object "unavaliable-rest-arg")
			   results)))))
      (di:lambda-list-unavailable
       ()
       (push (make-unprintable-object "lambda-list-unavailable") results)))
    (prin1 (mapcar #'ensure-printable-object (nreverse results)))
    (when (di:debug-function-kind d-fun)
      (write-char #\[)
      (prin1 (di:debug-function-kind d-fun))
      (write-char #\]))))

(defun ensure-printable-object (object)
  (handler-case
      (with-open-stream (out (make-broadcast-stream))
	(prin1 object out)
	object)
    (error (cond)
      (declare (ignore cond))
      (make-unprintable-object "error printing object"))))

(defun frame-call-arg (var location frame)
  (lambda-var-dispatch var location
    (make-unprintable-object "unused-arg")
    (di:debug-variable-value var frame)
    (make-unprintable-object "unavailable-arg")))

;;; PRINT-FRAME-CALL -- Interface
;;;
;;; This prints a representation of the function call causing frame to exist.
;;; Verbosity indicates the level of information to output; zero indicates just
;;; printing the debug-function's name, and one indicates displaying call-like,
;;; one-liner format with argument values.
;;;
(defun print-frame-call (frame &key
			       ((:print-length *print-length*)
				(or *debug-print-length* *print-length*))
			       ((:print-level *print-level*)
				(or *debug-print-level* *print-level*))
			       (verbosity 1)
			       (number nil))
  (cond
   ((zerop verbosity)
    (when number
      (format t "~&~S: " (di:frame-number frame)))
    (format t "~S" frame))
   (t
    (when number
      (format t "~&~S: " (di:frame-number frame)))
    (print-frame-call-1 frame)))
  (when (>= verbosity 2)
    (let ((loc (di:frame-code-location frame)))
      (handler-case
	  (progn
	    (di:code-location-debug-block loc)
	    (format t "~%Source: ")
	    (print-code-location-source-form loc 0))
	(di:debug-condition (ignore) ignore)
	(error (cond) (format t "Error finding source: ~A" cond))))))


;;;; Invoke-debugger.

(defvar *debugger-hook* nil
  "This is either nil or a function of two arguments, a condition and the
   value of *debugger-hook*.  This function can either handle the condition
   or return which causes the standard debugger to execute.  The system
   passes the value of this variable to the function because it binds
   *debugger-hook* to nil around the invocation.")

;;; These are bound on each invocation of INVOKE-DEBUGGER.
;;;
(defvar *debug-restarts*)
(defvar *debug-condition*)

;;; INVOKE-DEBUGGER -- Public.
;;;
(defun invoke-debugger (condition)
  "The debugger.  Type h for help."
  (when *debugger-hook*
    (let ((hook *debugger-hook*)
	  (*debugger-hook* nil))
      (funcall hook condition hook)))
  (unix:unix-sigsetmask 0)
  (let* ((*debug-condition* condition)
	 (*debug-restarts* (compute-restarts condition))
	 (*standard-input* *debug-io*)		; in case of setq
	 (*standard-output* *debug-io*)		; ''  ''  ''  ''
	 (*error-output* *debug-io*)
	 ;; Rebind some printer control variables.
	 (kernel:*current-level* 0)
	 (*print-readably* nil)
	 (*read-eval* t))
    (format *error-output* "~2&~A~2&" *debug-condition*)
    (or (typep condition 'step-condition)
	(show-restarts *debug-restarts* *error-output*))
    (internal-debug)))

;;; SHOW-RESTARTS -- Internal.
;;;
(defun show-restarts (restarts &optional (s *error-output*))
  (when restarts
    (format s "~&Restarts:~%")
    (let ((count 0)
	  (names-used '(nil))
	  (max-name-len 0))
      (dolist (restart restarts)
	(let ((name (restart-name restart)))
	  (when name
	    (let ((len (length (princ-to-string name))))
	      (when (> len max-name-len)
		(setf max-name-len len))))))
      (unless (zerop max-name-len)
	(incf max-name-len 3))
      (dolist (restart restarts)
	(let ((name (restart-name restart)))
	  (cond ((member name names-used)
		 (format s "~& ~2D: ~@VT~A~%" count max-name-len restart))
		(t
		 (format s "~& ~2D: [~VA] ~A~%"
			 count (- max-name-len 3) name restart)
		 (push name names-used))))
	(incf count)))))

;;; INTERNAL-DEBUG -- Internal Interface.
;;;
;;; This calls DEBUG-LOOP, performing some simple initializations before doing
;;; so.  INVOKE-DEBUGGER calls this to actually get into the debugger.
;;; CONDITIONS::ERROR-ERROR calls this in emergencies to get into a debug
;;; prompt as quickly as possible with as little risk as possible for stepping
;;; on whatever is causing recursive errors.
;;;
(defun internal-debug ()
  (let ((*in-the-debugger* t)
	(*read-suppress*))
    (unless (typep *debug-condition* 'step-condition)
      (clear-input *debug-io*)
      (format *debug-io* "~2&Debug  (type H for help)~2%"))
    #-mp (debug-loop)
    #+mp (mp:without-scheduling (debug-loop))))


;;;; Debug-loop.

#[ The Debugger Command Loop

The debugger is an interactive read-eval-print loop much like the normal
top-level, but some symbols are interpreted as debugger commands instead of
being evaluated.  A debugger command starts with the symbol name of the
command, possibly followed by some arguments on the same line.  Some
commands prompt for additional input.  Debugger commands can be abbreviated
by any unique prefix: "help" can be typed as "h", "he", etc.  For
convenience, some commands have ambiguous one-letter abbreviations: "f" for
"frame".

The package is not significant in debugger commands; any symbol with the
name of a debugger command will work.  If you want to show the value of a
variable that happens also to be the name of a debugger command, you can
use the "list-locals" command or the `debug:var' function, or you can wrap
the variable in a `progn' to hide it from the command loop.

The debugger prompt is "<frame>]", where <frame> is the number of the
current frame.  Frames are numbered starting from zero at the top (most
recent call), increasing down to the bottom.  The current frame is the
frame that commands refer to.  The current frame also provides the lexical
environment for evaluation of forms other than debugger commands.

\cpsubindex{evaluation}{debugger} The debugger evaluates forms in the
lexical environment of the functions being debugged.  The debugger can only
access variables.  You can't \code{go} or \code{return-from} into a
function, and you can't call local functions.  Special variable references
are evaluated with their current value (the innermost binding around the
debugger invocation) -- you don't get the value that the special had in the
current frame.  \xlref{debug-vars} for more information on debugger
variable access.
]#

(defvar *flush-debug-errors* t
  "When set, avoid calling INVOKE-DEBUGGER recursively when errors occur
   while executing in the debugger.  The 'flush' command toggles this.")

(defvar *debug-readtable* nil
  "When true, becomes the system *READTABLE* in the debugger
   read-eval-print loop")

(defun debug-loop ()
  (let* ((*debug-command-level* (1+ *debug-command-level*))
	 (*real-stack-top* (di:top-frame))
	 (*stack-top* (or *stack-top-hint* *real-stack-top*))
	 (*stack-top-hint* nil)
	 (*current-frame* *stack-top*)
	 (*readtable* (or *debug-readtable* *readtable*)))
    (handler-bind ((di:debug-condition #'(lambda (condition)
					   (princ condition *debug-io*)
					   (throw 'debug-loop-catcher nil))))
      (fresh-line)
      (print-frame-call *current-frame* :verbosity 2)
      (loop
	(catch 'debug-loop-catcher
	  (handler-bind ((error #'(lambda (condition)
				    (when *flush-debug-errors*
				      (clear-input *debug-io*)
				      (princ condition)
				      (format t "~&Error flushed ...")
				      (throw 'debug-loop-catcher nil)))))
	    ;; Must bind level for restart function created by
	    ;; WITH-SIMPLE-RESTART.
	    (let ((level *debug-command-level*)
		  (restart-commands (make-restart-commands)))
	      (with-simple-restart (abort "Return to debug level ~D." level)
		(funcall *debug-prompt*)
		(let ((input (ext:get-stream-command *debug-io*)))
		  (cond (input
			 (let ((cmd-fun (debug-command-p
					 (ext:stream-command-name input)
					 restart-commands)))
			   (cond
			    ((not cmd-fun)
			     (error "Unknown stream-command -- ~S." input))
			    ((consp cmd-fun)
			     (error "Ambiguous debugger command: ~S." cmd-fun))
			    (t
			     (apply cmd-fun (ext:stream-command-args input))))))
			(t
			 (let* ((exp (read))
				(cmd-fun (debug-command-p exp restart-commands)))
			   (cond ((not cmd-fun)
				  (debug-eval-print exp))
				 ((consp cmd-fun)
				  (format t "~&The command ~S is ambiguous:~%"
					  exp)
				  (dolist (ele cmd-fun)
				    (format t "   ~A~%" ele)))
				 (t
				  (funcall cmd-fun)))))))))))))))

(defvar *auto-eval-in-frame* t
  "When set (the default), evaluations in the debugger's command loop occur
   relative to the current frame's environment without the need of debugger
   forms that explicitly control this kind of evaluation.")

(defun debug-eval-print (exp)
  (setq +++ ++ ++ + + - - exp)
  (let* ((values (multiple-value-list
		  (if (and (fboundp 'compile) *auto-eval-in-frame*)
		      (di:eval-in-frame *current-frame* -)
		      (eval -))))
	 (*standard-output* *debug-io*))
    (fresh-line)
    (if values (prin1 (car values)))
    (dolist (x (cdr values))
      (fresh-line)
      (prin1 x))
    (setq /// // // / / values)
    (setq *** ** ** * * (car values))
    ;; Make sure nobody passes back an unbound marker.
    (unless (boundp '*)
      (setq * nil)
      (fresh-line)
      (princ "Setting * to NIL -- was unbound marker."))))


;;;; Debug loop functions.

;;; These commands are functions, not really commands, so users can get
;;; their hands on the values returned.

(eval-when (eval compile)

(defmacro define-var-operation (ref-or-set &optional value-var)
  `(let* ((temp (etypecase name
		  (symbol (di:debug-function-symbol-variables
			   (di:frame-debug-function *current-frame*)
			   name))
		  (simple-string (di:ambiguous-debug-variables
				  (di:frame-debug-function *current-frame*)
				  name))))
	  (location (di:frame-code-location *current-frame*))
	  ;; Let's only deal with valid variables.
	  (vars (keep-if #'(lambda (v)
			     (eq (di:debug-variable-validity v location)
				 :valid))
			 temp)))
     (declare (list vars))
     (cond ((null vars)
	    (error "No known valid variables match ~S." name))
	   ((= (length vars) 1)
	    ,(ecase ref-or-set
	       (:ref
		'(di:debug-variable-value (car vars) *current-frame*))
	       (:set
		`(setf (di:debug-variable-value (car vars) *current-frame*)
		       ,value-var))))
	   (t
	    ;; Since we have more than one, first see if we have any
	    ;; variables that exactly match the specification.
	    (let* ((name (etypecase name
			   (symbol (symbol-name name))
			   (simple-string name)))
		   (exact (keep-if #'(lambda (v)
				       (string= (di:debug-variable-name v)
						name))
				   vars))
		   (vars (or exact vars)))
	      (declare (simple-string name)
		       (list exact vars))
	      (cond
	       ;; Check now for only having one variable.
	       ((= (length vars) 1)
		,(ecase ref-or-set
		   (:ref
		    '(di:debug-variable-value (car vars) *current-frame*))
		   (:set
		    `(setf (di:debug-variable-value (car vars) *current-frame*)
			   ,value-var))))
	       ;; If there weren't any exact matches, flame about ambiguity
	       ;; unless all the variables have the same name.
	       ((and (not exact)
		     (find-if-not
		      #'(lambda (v)
			  (string= (di:debug-variable-name v)
				   (di:debug-variable-name (car vars))))
		      (cdr vars)))
		(error "Specification ambiguous:~%~{   ~A~%~}"
		       (mapcar #'di:debug-variable-name
			       (delete-duplicates
				vars :test #'string=
				:key #'di:debug-variable-name))))
	       ;; All names are the same, so see if the user ID'ed one of them.
	       (id-supplied
		(let ((v (find id vars :key #'di:debug-variable-id)))
		  (unless v
		    (error "Invalid variable ID, ~D, should have been one of ~S."
			   id (mapcar #'di:debug-variable-id vars)))
		  ,(ecase ref-or-set
		     (:ref
		      '(di:debug-variable-value v *current-frame*))
		     (:set
		      `(setf (di:debug-variable-value v *current-frame*)
			     ,value-var)))))
	       (t
		(error "Specify variable ID to disambiguate ~S.  Use one of ~S."
		       name (mapcar #'di:debug-variable-id vars)))))))))

) ;EVAL-WHEN

;;; VAR -- Public.
;;;
(defun var (name &optional (id 0 id-supplied))
  "Return the value of the variable in the current frame named $name.
   $name is a simple-string or symbol.

   When $name is a symbol, interpret it as the symbol name of the variable,
   i.e. the package is significant.  If $name is an uninterned symbol
   (`gensym'), then return the value of the uninterned variable with the
   same name.  If $name is a string, interpret it as the prefix of a
   variable name, which must uniquely complete to the name of a valid
   variable.

   If name is the initial substring of variables with ambiguous names, then
   simply displayi the ambiguous names.  If name determines multiple
   variables with the same name, and $id is specified return the name
   indexed by $id.  If $id was left out, then simply displaying the
   distinguishing id values.

   This function is useful mainly for accessing the value of uninterned or
   ambiguous variables, since most variables can be evaluated directly.

   The result of this function is limited to the availability of variable
   information.  This is `setf'able."
  (define-var-operation :ref))
;;;
(defun (setf var) (value name &optional (id 0 id-supplied))
  (define-var-operation :set value))

;;; ARG -- Public.
;;;
(defun arg (n)
  "Returns the n'th argument's value if possible.  Argument zero is the first
   argument in a frame's default printed representation.  Count keyword/value
   pairs as separate arguments."
  (multiple-value-bind
      (var lambda-var-p)
      (nth-arg n (handler-case (di:debug-function-lambda-list
				(di:frame-debug-function *current-frame*))
		   (di:lambda-list-unavailable ()
		     (error "No argument values are available."))))
    (if lambda-var-p
	(lambda-var-dispatch var (di:frame-code-location *current-frame*)
	  (error "Unused arguments have no values.")
	  (di:debug-variable-value var *current-frame*)
	  (error "Invalid argument value."))
	var)))

;;; NTH-ARG -- Internal.
;;;
;;; This returns the n'th arg as the user sees it from args, the result of
;;; DI:DEBUG-FUNCTION-LAMBDA-LIST.  If this returns a potential debug-variable
;;; from the lambda-list, then the second value is t.  If this returns a
;;; keyword symbol or a value from a rest arg, then the second value is nil.
;;;
(defun nth-arg (count args)
  (let ((n count))
    (dolist (ele args (error "Argument specification out of range -- ~S." n))
      (lambda-list-element-dispatch ele
	:required ((if (zerop n) (return (values ele t))))
	:optional ((if (zerop n) (return (values (second ele) t))))
	:keyword ((cond ((zerop n)
			 (return (values (second ele) nil)))
			((zerop (decf n))
			 (return (values (third ele) t)))))
	:deleted ((if (zerop n) (return (values ele t))))
	:rest ((let ((var (second ele)))
		 (lambda-var-dispatch var
				      (di:frame-code-location *current-frame*)
		   (error "Unused rest-arg before n'th argument.")
		   (dolist (value
			    (di:debug-variable-value var *current-frame*)
			    (error "Argument specification out of range -- ~S."
				   n))
		     (if (zerop n)
			 (return-from nth-arg (values value nil))
			 (decf n)))
		   (error "Invalid rest-arg before n'th argument.")))))
      (decf n))))


;;;; Debug loop command definition.

(defvar *debug-commands* nil)

;;; DEF-DEBUG-COMMAND -- Internal.
;;;
;;; Interface to *debug-commands*.  No required arguments in args are
;;; permitted.
;;;
(defmacro def-debug-command (name args &rest body)
  (let ((fun-name (intern (concatenate 'simple-string name "-DEBUG-COMMAND"))))
    `(progn
       (when (assoc ,name *debug-commands* :test #'string=)
	 (setf *debug-commands*
	       (remove ,name *debug-commands* :key #'car :test #'string=)))
       (defun ,fun-name ,args
	 (or *in-the-debugger*
	     (error "Invoking debugger command while outside the debugger."))
	 ,@body)
       (push (cons ,name #',fun-name) *debug-commands*)
       ',fun-name)))

;;; DEF-DEBUG-COMMAND-ALIAS -- Internal.
;;;
(defun def-debug-command-alias (new-name existing-name)
  (let ((pair (assoc existing-name *debug-commands* :test #'string=)))
    (or pair (error "Unknown debug command name -- ~S" existing-name))
    (push (cons new-name (cdr pair)) *debug-commands*))
  new-name)

;;; DEBUG-COMMAND-P -- Internal.
;;;
;;; This takes a symbol and uses its name to find a debugger command, using
;;; initial substring matching.  It returns the command function if form
;;; identifies only one command, but if form is ambiguous, this returns a list
;;; of the command names.  If there are no matches, this returns nil.  Whenever
;;; the loop that looks for a set of possibilities encounters an exact name
;;; match, we return that command function immediately.
;;;
(defun debug-command-p (form &optional other-commands)
  (if (or (symbolp form) (integerp form))
      (let* ((name
	      (if (symbolp form)
		  (symbol-name form)
		  (format nil "~d" form)))
	     (len (length name))
	     (res nil))
	(declare (simple-string name)
		 (fixnum len)
		 (list res))
	;;
	;; Find matching commands, punting if exact match.
	(flet ((match-command (ele)
	         (let* ((str (car ele))
			(str-len (length str)))
		   (declare (simple-string str)
			    (fixnum str-len))
		   (cond ((< str-len len))
			 ((= str-len len)
			  (when (string= name str :end1 len :end2 len)
			    (return-from debug-command-p (cdr ele))))
			 ((string= name str :end1 len :end2 len)
			  (push ele res))))))
	  (mapc #'match-command *debug-commands*)
	  (mapc #'match-command other-commands))
	;;
	;; Return the right value.
	(cond ((not res) nil)
	      ((= (length res) 1)
	       (cdar res))
	      (t ;Just return the names.
	       (do ((cmds res (cdr cmds)))
		   ((not cmds) res)
		 (setf (car cmds) (caar cmds))))))))

;;; Returns a list of debug commands (in the same format as *debug-commands*)
;;; that invoke each active restart.
;;;
;;; Two commands are made for each restart: one for the number, and one for
;;; the restart name (unless it's been shadowed by an earlier restart of the
;;; same name.
;;;
(defun make-restart-commands (&optional (restarts *debug-restarts*))
  (let ((commands)
	(num 0))			; better be the same as show-restarts!
    (dolist (restart restarts)
      (let ((name (string (restart-name restart))))
	(unless (find name commands :key #'car :test #'string=)
	  (let ((restart-fun
		 #'(lambda ()
		     (invoke-restart-interactively restart))))
	    (push (cons name restart-fun) commands)
	    (push (cons (format nil "~d" num) restart-fun) commands))))
      (incf num))
    commands))


;;;; Frame changing commands.

(def-debug-command "UP" ()
  (let ((next (di:frame-up *current-frame*)))
    (cond (next
	   (setf *current-frame* next)
	   (print-frame-call next))
	  (t
	   (format t "~&Top of stack.")))))

(def-debug-command "DOWN" ()
  (let ((next (di:frame-down *current-frame*)))
    (cond (next
	   (setf *current-frame* next)
	   (print-frame-call next))
	  (t
	   (format t "~&Bottom of stack.")))))

(def-debug-command-alias "D" "DOWN")

(def-debug-command "TOP" ()
  (do ((prev *current-frame* lead)
       (lead (di:frame-up *current-frame*) (di:frame-up lead)))
      ((null lead)
       (setf *current-frame* prev)
       (print-frame-call prev))))

(def-debug-command "BOTTOM" ()
  (do ((prev *current-frame* lead)
       (lead (di:frame-down *current-frame*) (di:frame-down lead)))
      ((null lead)
       (setf *current-frame* prev)
       (print-frame-call prev))))

(def-debug-command-alias "B" "BOTTOM")

(def-debug-command "FRAME" (&optional
			    (n (read-prompting-maybe "Frame number: ")))
  (let ((current (di:frame-number *current-frame*)))
    (cond ((= n current)
	   (princ "You are here."))
	  ((> n current)
	   (print-frame-call
	    (setf *current-frame*
		  (do ((prev *current-frame* lead)
		       (lead (di:frame-down *current-frame*)
			     (di:frame-down lead)))
		      ((null lead)
		       (princ "Bottom of stack encountered.")
		       prev)
		    (when (= n (di:frame-number prev))
		      (return prev))))))
	  (t
	   (print-frame-call
	    (setf *current-frame*
		  (do ((prev *current-frame* lead)
		       (lead (di:frame-up *current-frame*)
			     (di:frame-up lead)))
		      ((null lead)
		       (princ "Top of stack encountered.")
		       prev)
		    (when (= n (di:frame-number prev))
		      (return prev)))))))))

(def-debug-command-alias "F" "FRAME")


;;;; In and Out commands.

;; FIX think lost args to these commands
#[ Exiting Commands

These commands exit the debugger.

  % quit

    Throw to top level.

  % restart

    Invoke the n'th restart case as displayed by the "error" command.  If n
    is left out, report the available restart cases.

  % go

    Call "continue" on the condition given to `debug'.  If there is no
    restart case named "continue", then signal an error.

  % abort

    Calls `abort' on the condition given to `debug'.  This is useful for
    popping debug command loop levels or aborting to top level, as the case
    may be.
]#
#| Hidden from the description above

  % debug:debug-return} \var{expression} \mopt{\var{frame}})

    From the current or specified frame, return the result of evaluating
    expression.  If multiple values are expected, then this function should
    be called for multiple values.
|#

(def-debug-command "QUIT" ()
  (throw 'lisp::top-level-catcher nil))

(def-debug-command "GO" ()
  (continue *debug-condition*)
  (error "No restart named continue."))

(def-debug-command "RESTART" ()
  (let ((num (read-if-available :prompt)))
    (when (eq num :prompt)
      (show-restarts *debug-restarts*)
      (write-string "Restart: ")
      (force-output)
      (setf num (read *standard-input*)))
    (let ((restart (typecase num
		     (unsigned-byte
		      (nth num *debug-restarts*))
		     (symbol
		      (find num *debug-restarts* :key #'restart-name
			    :test #'(lambda (sym1 sym2)
				      (string= (symbol-name sym1)
					       (symbol-name sym2)))))
		     (t
		      (format t "~S is invalid as a restart name.~%" num)
		      (return-from restart-debug-command nil)))))
      (if restart
	  (invoke-restart-interactively restart)
	  (princ "No such restart.")))))


;;;; Information commands.

#[ Information Commands

Most of these commands print information about the current frame or
function, but a few show general information.

  % help, ?

    Display a synopsis of debugger commands.

  % describe

    Call `describe' on the current function, display number of local
    variables, and indicate whether the function is compiled or
    interpreted.

  % print

    Display the current function call as it would be displayed by moving to
    this frame.

  % vprint (or pp) verbosity*

    Displays the current function call using *print-level* and
    *print-length* instead of *debug-print-level* and *debug-print-length*.
    $verbosity is a small integer (fallback 2) that controls other
    dimensions of verbosity.

  % error

    Prints the condition given to `invoke-debugger' and the active proceed
    cases.

  % backtrace n*

    Display all the frames from the current to the bottom.  Only show $n
    frames, if $n is specified.  The printing is controlled by
    *debug-print-level* and *debug-print-length*.
]#
#| Hidden in description above:

  % (debug:debug-function n*)

    Return the function from the current or specified frame.

  % (debug:function-name n*)

    Return the function name from the current or specified frame.

  % (debug:pc frame*)

    Return the index of the instruction for the function in the current or
    specified frame.  This is useful in conjunction with `disassemble'.
    The pc returned points to the instruction after the one that was fatal.
|#
(defvar *help-line-scroll-count* 20
  "This controls how many lines the debugger's help command prints before
   printing a prompting line to continue with output.")

(def-debug-command "HELP" ()
  (let* ((end -1)
	 (len (length debug-help-string))
	 (len-1 (1- len)))
    (loop
      (let ((start (1+ end))
	    (count *help-line-scroll-count*))
	(loop
	  (setf end (position #\newline debug-help-string :start (1+ end)))
	  (cond ((or (not end) (= end len-1))
		 (setf end len)
		 (return))
		((or (zerop (decf count)) (= end len))
		 (return))))
	(write-string debug-help-string *standard-output*
		      :start start :end end))
      (when (= end len) (return))
      (format t "~%[RETURN FOR MORE, Q TO QUIT HELP TEXT]: ")
      (force-output)
      (let ((res (read-line)))
	(when (or (string= res "q") (string= res "Q"))
	  (return))))))

(def-debug-command-alias "?" "HELP")

(def-debug-command "ERROR" ()
  (format t "~A~%" *debug-condition*)
  (show-restarts *debug-restarts*))

(def-debug-command "BACKTRACE" ()
  (backtrace (read-if-available most-positive-fixnum)))

(def-debug-command "PRINT" ()
  (print-frame-call *current-frame*))

(def-debug-command-alias "P" "PRINT")

(def-debug-command "VPRINT" ()
  (print-frame-call *current-frame* :print-level nil :print-length nil
		    :verbosity (read-if-available 2)))

(def-debug-command-alias "PP" "VPRINT")

(def-debug-command "LIST-LOCALS" ()
  (let ((d-fun (di:frame-debug-function *current-frame*)))
    (if (di:debug-variable-info-available d-fun)
	(let ((*print-level* (or *debug-print-level* *print-level*))
	      (*print-length* (or *debug-print-length* *print-length*))
	      (*standard-output* *debug-io*)
	      (location (di:frame-code-location *current-frame*))
	      (prefix (read-if-available nil))
	      (any-p nil)
	      (any-valid-p nil))
	  (dolist (v (di:ambiguous-debug-variables
			d-fun
			(if prefix (string prefix) "")))
	    (setf any-p t)
	    (when (eq (di:debug-variable-validity v location) :valid)
	      (setf any-valid-p t)
	      (format t "~S~:[#~D~;~*~]  =  ~S~%"
		      (di:debug-variable-symbol v)
		      (zerop (di:debug-variable-id v))
		      (di:debug-variable-id v)
		      (di:debug-variable-value v *current-frame*))))

	  (cond
	   ((not any-p)
	    (format t "No local variables ~@[starting with ~A ~]~
	               in function."
		    prefix))
	   ((not any-valid-p)
	    (format t "All variables ~@[starting with ~A ~]currently ~
	               have invalid values."
		    prefix))))
	(write-line "No variable information available."))))

(def-debug-command-alias "L" "LIST-LOCALS")

(def-debug-command "SOURCE" ()
  (fresh-line)
  (print-code-location-source-form (di:frame-code-location *current-frame*)
				   (read-if-available 0)))

(def-debug-command "VSOURCE" ()
  (fresh-line)
  (print-code-location-source-form (di:frame-code-location *current-frame*)
				   (read-if-available 0)
				   t))


;;;; Source location printing.

;;; We cache a stream to the last valid file debug source so that we won't have
;;; to repeatedly open the file.
;;;
(defvar *cached-debug-source* nil)
(declaim (type (or di:debug-source null) *cached-debug-source*))
(defvar *cached-source-stream* nil)
(declaim (type (or stream null) *cached-source-stream*))

;;; To suppress the read-time evaluation #. macro during source read
;;; the *readtable* is modified. The *readtable* is cached to avoid
;;; copying it each time, and invalidated when the
;;; *cached-debug-source* has changed.
(defvar *cached-readtable* nil)
(declaim (type (or readtable null) *cached-readtable*))

(pushnew #'(lambda ()
	     (setq *cached-debug-source* nil *cached-source-stream* nil
		   *cached-readtable* nil))
	 ext:*before-save-initializations*)

;;; We also cache the last top-level form that we printed a source for so that
;;; we don't have to do repeated reads and calls to FORM-NUMBER-TRANSLATIONS.
;;;
(defvar *cached-top-level-form-offset* nil)
(declaim (type (or kernel:index null) *cached-top-level-form-offset*))
(defvar *cached-top-level-form*)
(defvar *cached-form-number-translations*)

;;; GET-TOP-LEVEL-FORM  --  Internal
;;;
;;;    Given a code location, return the associated form-number translations
;;; and the actual top-level form.  We check our cache --- if there is a miss,
;;; we dispatch on the kind of the debug source.
;;;
(defun get-top-level-form (location)
  (let ((d-source (di:code-location-debug-source location)))
    (if (and (eq d-source *cached-debug-source*)
	     (eql (di:code-location-top-level-form-offset location)
		  *cached-top-level-form-offset*))
	(values *cached-form-number-translations* *cached-top-level-form*)
	(let* ((offset (di:code-location-top-level-form-offset location))
	       (res
		(ecase (di:debug-source-from d-source)
		  (:file (get-file-top-level-form location))
		  ((:lisp :stream)
		   (svref (di:debug-source-name d-source) offset)))))
	  (setq *cached-top-level-form-offset* offset)
	  (values (setq *cached-form-number-translations*
			(di:form-number-translations res offset))
		  (setq *cached-top-level-form* res))))))

;;; GET-FILE-TOP-LEVEL-FORM -- Internal.
;;;
;;; Locates the source file (if it still exists) and grabs the top-level form.
;;; If the file is modified, we use the top-level-form offset instead of the
;;; recorded character offset.
;;;
(defun get-file-top-level-form (location)
  (let* ((d-source (di:code-location-debug-source location))
	 (tlf-offset (di:code-location-top-level-form-offset location))
	 (local-tlf-offset (- tlf-offset
			      (di:debug-source-root-number d-source)))
	 (char-offset
	  (aref (or (di:debug-source-start-positions d-source)
		    (error "No start positions map."))
		local-tlf-offset))
	 (name (di:debug-source-name d-source)))
    (or (eq d-source *cached-debug-source*)
	(unless (and *cached-source-stream*
		     (equal (pathname *cached-source-stream*)
			    (pathname name)))
	  (setq *cached-readtable* nil)
	  (when *cached-source-stream* (close *cached-source-stream*))
	  (setq *cached-source-stream* (open name :if-does-not-exist ()))
	  (or *cached-source-stream*
	      (error "Source file no longer exists:~%  ~A." (namestring name)))
	  (format t "~%; File: ~A~%" (namestring name)))

	(setq *cached-debug-source*
	      (if (= (di:debug-source-created d-source) (file-write-date name))
		  d-source nil)))

    (cond
     ((eq *cached-debug-source* d-source)
      (file-position *cached-source-stream* char-offset))
     (t
      (format t "~%; File has been modified since compilation:~%;   ~A~@
		 ; Using form offset instead of character position.~%"
	      (namestring name))
      (file-position *cached-source-stream* 0)
      (let ((*read-suppress* t))
	(dotimes (i local-tlf-offset)
	  (read *cached-source-stream*)))))
    (unless *cached-readtable*
      (setq *cached-readtable* (copy-readtable))
      (set-dispatch-macro-character
       #\# #\.
       #'(lambda (stream sub-char &rest rest)
	   (declare (ignore rest sub-char))
	   (let ((token (read stream t nil t)))
	     (format nil "#.~s" token)))
       *cached-readtable*))
    (let ((*readtable* *cached-readtable*))
      (read *cached-source-stream*))))

;;; PRINT-CODE-LOCATION-SOURCE-FORM -- Internal.
;;;
(defun print-code-location-source-form (location context &optional verbose)
  (let* ((location (maybe-block-start-location location))
	 (*print-level* (if verbose
			    nil
			    (or *debug-print-level* *print-level*)))
	 (*print-length* (if verbose
			     nil
			     (or *debug-print-length* *print-length*)))
	 (form-num (di:code-location-form-number location)))
    (multiple-value-bind (translations form)
			 (get-top-level-form location)
      (or (< form-num (length translations))
	  (error "Source path no longer exists."))
      (prin1 (di:source-path-context form
				     (svref translations form-num)
				     context)))))


;;;; Breakpoint and step commands.

#[ Breakpoint Commands

Breakpoints can be set inside compiled functions and stepping of compiled
code.  Breakpoints can only be set at known locations
(\pxlref{unknown-locations}), so these commands are mostly only useful when
the \code{debug} optimize quality is at least 2 (\pxlref{debugger-policy}).
These commands manipulate breakpoints:

  % breakpoint location [option value]*

    Set a breakpoint in some function.  $location may be an integer code
    location number (as displayed by "list-locations") or a keyword.  The
    keyword can be used to indicate setting a breakpoint at the function
    start (:start, :s) or function end (:end, :e).  The options can
    :condition, :break, :print and :function, and work similarly to the
    options in the `trace' function.

  % list-locations (or ll) function*

    List all the code locations in the current frame's function, or in
    $function if it is supplied.  The display format is the code location
    number, a colon and then the source form for that location:

        3: (1- N)

    If consecutive locations have the same source, then a numeric range
    like \code{3-5:} will be printed.  For example, a default function call
    has a known location both immediately before and after the call, which
    would result in two code locations with the same source.  The listed
    function becomes the new default function for breakpoint setting (via
    the "breakpoint" command).

  % list-breakpoints (or lb)

    List all currently active breakpoints with their breakpoint number.

  % delete-breakpoint (or db) number*

    Delete a breakpoint specified by its breakpoint number.  If number is
    left out, delete all breakpoints.

  % step

    Step to the next possible breakpoint location in the current function.
    This always steps over function calls, instead of stepping into them.

[ Breakpoint Example ]
]#

;;; Steps to the next code-location
(def-debug-command "STEP" ()
  (setf *number-of-steps* (read-if-available 1))
  (set-step-breakpoint *current-frame*)
  (continue *debug-condition*)
  (error "Couldn't continue."))

;;; Lists possible breakpoint locations, which are active, and where go will
;;; continue.  Sets *possible-breakpoints* to the code-locations which can then
;;; be used by sbreakpoint.  Takes a function as an optional argument.
(def-debug-command "LIST-LOCATIONS" ()
  (let ((df (read-if-available *default-breakpoint-debug-function*)))
    (cond ((consp df)
	   (setf df (di:function-debug-function (eval df)))
	   (setf *default-breakpoint-debug-function* df))
	  ((or (eq ':c df)
	       (not *default-breakpoint-debug-function*))
	   (setf df (di:frame-debug-function *current-frame*))
	   (setf *default-breakpoint-debug-function* df)))
    (setf *possible-breakpoints* (possible-breakpoints df)))
  (let ((continue-at (di:frame-code-location *current-frame*)))
    (let ((active (location-in-list *default-breakpoint-debug-function*
				    *breakpoints* :function-start))
	  (here (di:code-location=
		 (di:debug-function-start-location
		  *default-breakpoint-debug-function*) continue-at)))
      (when (or active here)
	(format t "::FUNCTION-START ")
	(when active (format t " *Active*"))
	(when here (format t " *Continue here*"))))

    (let ((prev-location nil)
	  (prev-num 0)
	  (this-num 0))
      (flet ((flush ()
	       (when prev-location
		 (let ((this-num (1- this-num)))
		   (if (= prev-num this-num)
		       (format t "~&~D: " prev-num)
		       (format t "~&~D-~D: " prev-num this-num)))
		 (print-code-location-source-form prev-location 0)
		 (when *print-location-kind*
		   (format t "~S " (di:code-location-kind prev-location)))
		 (when (location-in-list prev-location *breakpoints*)
		   (format t " *Active*"))
		 (when (di:code-location= prev-location continue-at)
		   (format t " *Continue here*")))))

	(dolist (code-location *possible-breakpoints*)
	  (when (or *print-location-kind*
		    (location-in-list code-location *breakpoints*)
		    (di:code-location= code-location continue-at)
		    (not prev-location)
		    (not (eq (di:code-location-debug-source code-location)
			     (di:code-location-debug-source prev-location)))
		    (not (eq (di:code-location-top-level-form-offset
			      code-location)
			     (di:code-location-top-level-form-offset
			      prev-location)))
		    (not (eq (di:code-location-form-number code-location)
			     (di:code-location-form-number prev-location))))
	    (flush)
	    (setq prev-location code-location  prev-num this-num))

	  (incf this-num))))

    (when (location-in-list *default-breakpoint-debug-function* *breakpoints*
			    :function-end)
      (format t "~&::FUNCTION-END *Active* "))))

(def-debug-command-alias "LL" "LIST-LOCATIONS")

;;; set breakpoint at given number
(def-debug-command "BREAKPOINT" ()
  (let ((index (read-prompting-maybe "Location number, :start, or :end: "))
	(break t)
	(condition t)
	(print nil)
	(print-functions nil)
	(function nil)
	(bp)
	(place *default-breakpoint-debug-function*))
    (flet ((get-command-line ()
	     (let ((command-line nil)
		   (unique '(nil)))
	       (loop
		 (let ((next-input (read-if-available unique)))
		   (when (eq next-input unique) (return))
		   (push next-input command-line)))
	       (nreverse command-line)))
	   (set-vars-from-command-line (command-line)
	     (do ((arg (pop command-line) (pop command-line)))
		 ((not arg))
	       (ecase arg
		 (:condition (setf condition (pop command-line)))
		 (:print (push (pop command-line) print))
		 (:break (setf break (pop command-line)))
		 (:function
		  (setf function (eval (pop command-line)))
		  (setf *default-breakpoint-debug-function*
			(di:function-debug-function function))
                  (setf place *default-breakpoint-debug-function*)
		  (setf *possible-breakpoints*
			(possible-breakpoints
			 *default-breakpoint-debug-function*))))))
	   (setup-function-start ()
	     (let ((code-loc (di:debug-function-start-location place)))
	       (setf bp (di:make-breakpoint #'main-hook-function place
					    :kind :function-start))
	       (setf break (di:preprocess-for-eval break code-loc))
	       (setf condition (di:preprocess-for-eval condition code-loc))
	       (dolist (form print)
		 (push (cons (di:preprocess-for-eval form code-loc) form)
		       print-functions))))
	   (setup-function-end ()
	     (setf bp
		   (di:make-breakpoint #'main-hook-function place
					  :kind :function-end))
	     (setf break
		   (coerce `(lambda (dummy)
				    (declare (ignore dummy)) ,break)
				 'function))
	     (setf condition (coerce `(lambda (dummy)
					(declare (ignore dummy)) ,condition)
				     'function))
	     (dolist (form print)
	       (push (cons
		      (coerce `(lambda (dummy)
				 (declare (ignore dummy)) ,form) 'function)
		      form)
		     print-functions)))
	   (setup-code-location ()
	     (setf place (nth index *possible-breakpoints*))
	     (setf bp (di:make-breakpoint #'main-hook-function place
					  :kind :code-location))
	     (dolist (form print)
	       (push (cons
		      (di:preprocess-for-eval form place)
		      form)
		     print-functions))
	     (setf break (di:preprocess-for-eval break place))
	     (setf condition (di:preprocess-for-eval condition place))))
      (set-vars-from-command-line (get-command-line))
      (cond
       ((or (eq index :start) (eq index :s))
	(setup-function-start))
       ((or (eq index :end) (eq index :e))
	(setup-function-end))
       (t
	(setup-code-location)))
      (di:activate-breakpoint bp)
      (let* ((new-bp-info (create-breakpoint-info place bp index
						  :break break
						  :print print-functions
						  :condition condition))
	     (old-bp-info (location-in-list new-bp-info *breakpoints*)))
	(when old-bp-info
	  (di:deactivate-breakpoint (breakpoint-info-breakpoint old-bp-info))
	  (setf *breakpoints* (remove old-bp-info *breakpoints*))
	  (format t "Note: previous breakpoint removed.~%"))
	(push new-bp-info *breakpoints*))
      (print-breakpoint-info (first *breakpoints*))
      (format t "~&Added."))))

(def-debug-command-alias "BP" "BREAKPOINT")

;;; list all breakpoints set
(def-debug-command "LIST-BREAKPOINTS" ()
  (setf *breakpoints*
	(sort *breakpoints* #'< :key #'breakpoint-info-breakpoint-number))
  (dolist (info *breakpoints*)
    (print-breakpoint-info info)))

(def-debug-command-alias "LB" "LIST-BREAKPOINTS")
(def-debug-command-alias "LBP" "LIST-BREAKPOINTS")

;;; remove breakpoint n or all if none given
(def-debug-command "DELETE-BREAKPOINT" ()
  (let* ((index (read-if-available nil))
	 (bp-info
	  (find index *breakpoints* :key #'breakpoint-info-breakpoint-number)))
    (cond (bp-info
	   (di:delete-breakpoint (breakpoint-info-breakpoint bp-info))
	   (setf *breakpoints* (remove bp-info *breakpoints*))
	   (format t "Breakpoint ~S removed.~%" index))
	  (index (format t "Breakpoint doesn't exist."))
	  (t
	   (dolist (ele *breakpoints*)
	     (di:delete-breakpoint (breakpoint-info-breakpoint ele)))
	   (setf *breakpoints* nil)
	   (format t "All breakpoints deleted.~%")))))

(def-debug-command-alias "DBP" "DELETE-BREAKPOINT")


;;;; Miscellaneous commands.

(def-debug-command "FLUSH-ERRORS" ()
  (if (setf *flush-debug-errors* (not *flush-debug-errors*))
      (write-line "Errors now flushed.")
      (write-line "Errors now create nested debug levels.")))

(def-debug-command "DESCRIBE" ()
  (let* ((curloc (di:frame-code-location *current-frame*))
	 (debug-fun (di:code-location-debug-function curloc))
	 (function (di:debug-function-function debug-fun)))
    (if function
	(describe function)
	(format t "Can't figure out the function for this frame."))))

(def-debug-command "INSPECT" (&optional
			      (variable (read-prompting-maybe "Variable: ")))
  (inspect::tty-inspect (var variable)))


;;;; Editor commands.

(def-debug-command "EDIT-SOURCE" ()
  (or (ed::ts-stream-p *terminal-io*)
      (error "The debugger's EDIT-SOURCE command only works in slave ~
	      Lisps connected to the editor."))
  (let* ((wire (ed::ts-stream-wire *terminal-io*))
	 (location (maybe-block-start-location
		    (di:frame-code-location *current-frame*)))
	 (d-source (di:code-location-debug-source location))
	 (name (di:debug-source-name d-source)))
    (ecase (di:debug-source-from d-source)
      (:file
       (let* ((tlf-offset (di:code-location-top-level-form-offset location))
	      (local-tlf-offset (- tlf-offset
				   (di:debug-source-root-number d-source)))
	      (char-offset (aref (or (di:debug-source-start-positions d-source)
				     (error "No start positions map."))
				 local-tlf-offset)))
	 (wire:remote wire
	   (ed::edit-source-location (namestring name)
				     (di:debug-source-created d-source)
				     tlf-offset local-tlf-offset char-offset
				     (di:code-location-form-number location)))
	 (wire:wire-force-output wire)))
      ((:lisp :stream)
       (wire:remote wire
	 (ed::cannot-edit-source-location))
       (wire:wire-force-output wire)))))

(def-debug-command "ED-HELP" ()
  (or (ed::ts-stream-p *terminal-io*)
      (error "The debugger's ED-HELP command only works in slave ~
	      Lisps connected to the editor."))
  (let* ((wire (ed::ts-stream-wire *terminal-io*)))
    (wire:remote wire
      (ed::info-command () "The Debugger"))
    (wire:wire-force-output wire)))


;;;; Debug loop command utilities.

(defun read-prompting-maybe (prompt &optional (in *standard-input*)
				    (out *standard-output*))
  (unless (ext:listen-skip-whitespace in)
    (princ prompt out)
    (force-output out))
  (read in))

(defun read-if-available (default &optional (stream *standard-input*))
  (if (ext:listen-skip-whitespace stream)
      (read stream)
      default))


#[ Breakpoint Example

Consider this definition of the factorial function:
    (defun ! (n)
      (if (zerop n)
	  1
	  (* n (! (1- n)))))

This debugger session demonstrates the use of breakpoints:

    common-lisp-user> (break) ; Invoke debugger

    Break

    Restarts:
      0: [CONTINUE] Return from BREAK.
      1: [ABORT   ] Return to Top-Level.

    Debug  (type H for help)

    (INTERACTIVE-EVAL (BREAK))
    0] ll #'!
    0: #'(LAMBDA (N) (BLOCK ! (IF # 1 #)))
    1: (ZEROP N)
    2: (* N (! (1- N)))
    3: (1- N)
    4: (! (1- N))
    5: (* N (! (1- N)))
    6: #'(LAMBDA (N) (BLOCK ! (IF # 1 #)))
    0] br 2
    (* N (! (1- N)))
    1: 2 in !
    Added.
    0] q

    common-lisp-user> (! 10) ; Call the function

    *Breakpoint hit*

    Restarts:
      0: [CONTINUE] Return from BREAK.
      1: [ABORT   ] Return to Top-Level.

    Debug  (type H for help)

    (! 10) ; We are now in first call (arg 10) before the multiply
    Source: (* N (! (1- N)))
    3] st

    *Step*

    (! 10) ; We have finished evaluation of (1- n)
    Source: (1- N)
    3] st

    *Breakpoint hit*

    Restarts:
      0: [CONTINUE] Return from BREAK.
      1: [ABORT   ] Return to Top-Level.

    Debug  (type H for help)

    (! 9) ; We hit the breakpoint in the recursive call
    Source: (* N (! (1- N)))
    3]
]#
