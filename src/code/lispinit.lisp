;;; Initialization, plus some other random functions that could do with a
;;; better place.

(in-package :lisp)

(export '(most-positive-fixnum most-negative-fixnum sleep
	  ++ +++ ** *** // ///))

(in-package :system)
(export '(compiler-version scrub-control-stack))

(in-package :extensions)
(export '(quit *prompt* prompt-long))

(in-package :lisp)

;;; Make the error system enable interrupts.

(defconstant most-positive-fixnum #.vm:target-most-positive-fixnum
  "The fixnum closest in value to positive infinity.")

(defconstant most-negative-fixnum #.vm:target-most-negative-fixnum
  "The fixnum closest in value to negative infinity.")


;;;; Random information.

;; Set for kernel.core.  Overwritten for lisp.core in worldload.lisp.
(defvar *version* #.(read-line (open "target:VERSION")))

;; FIX Set for kernel.core.  Overwritten for lisp.core in worldload.lisp.
(defvar *build-time* 0)

;;; Must be initialized in %INITIAL-FUNCTION before the DEFVAR runs.
(declaim
  #-gengc
  (special *gc-inhibit* *already-maybe-gcing*
	   *need-to-collect-garbage* *gc-verbose*
	   *before-gc-hooks* *after-gc-hooks*
	   #+x86 *pseudo-atomic-atomic*
	   #+x86 *pseudo-atomic-interrupted*
	   unix::*interrupts-enabled*
	   unix::*interrupt-pending*
	   *type-system-initialized*)
  #+gengc
  (special *gc-verbose* *before-gc-hooks* *after-gc-hooks*
	   *type-system-initialized*))


;;;; Random magic specials.

;;; These are filled in by Genesis.

#-gengc
(progn

(defvar *current-catch-block*)
(defvar *current-unwind-protect-block*)
(defvar *free-interrupt-context-index*)

); #-gengc progn


;;;; Random stuff that needs to be in the cold load which would otherwise be
;;;; byte-compiled.
;;;;
;(defvar edi::*in-the-editor* nil)

;;;; Called by defmacro expanders.

;;; VERIFY-KEYWORDS -- internal
;;;
;;; Determine if key-list is a valid list of keyword/value pairs.  Do not
;;; signal the error directly, 'cause we don't know how it should be signaled.
;;;
(defun verify-keywords (key-list valid-keys allow-other-keys)
  (do ((already-processed nil)
       (unknown-keyword nil)
       (remaining key-list (cddr remaining)))
      ((null remaining)
       (if (and unknown-keyword
		(not allow-other-keys)
		(not (lookup-keyword :allow-other-keys key-list)))
	   (values :unknown-keyword (list unknown-keyword valid-keys))
	   (values nil nil)))
    (cond ((not (and (consp remaining) (listp (cdr remaining))))
	   (return (values :dotted-list key-list)))
	  ((null (cdr remaining))
	   (return (values :odd-length key-list)))
	  ((member (car remaining) already-processed)
	   (return (values :duplicate (car remaining))))
	  ((or (eq (car remaining) :allow-other-keys)
	       (member (car remaining) valid-keys))
	   (push (car remaining) already-processed))
	  (t
	   (setf unknown-keyword (car remaining))))))

(defun lookup-keyword (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return (cadr remaining)))))
;;;
(defun keyword-supplied-p (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return t))))

(in-package "CONDITIONS")

(defvar *break-on-signals* nil
  "When (typep condition *break-on-signals*) is true, then calls to
   `signal' will enter the debugger prior to signalling that condition.")

(defun signal (datum &rest arguments)
  "Invokes the signal facility on a condition formed from datum and arguments.
   If the condition is not handled, nil is returned.  If
   (TYPEP condition *BREAK-ON-SIGNALS*) is true, the debugger is invoked before
   any signalling is done."
  (let ((condition (coerce-to-condition datum arguments
					'simple-condition 'signal))
        (*handler-clusters* *handler-clusters*))
    (let ((obos *break-on-signals*)
	  (*break-on-signals* nil))
      (when (typep condition obos)
	(break "~A~%Break entered because of *break-on-signals* (now NIL.)"
	       condition)))
    (loop
      (or *handler-clusters* (return))
      (let ((cluster (pop *handler-clusters*)))
	(dolist (handler cluster)
	  (when (typep condition (car handler))
	    (funcall (cdr handler) condition)))))
    nil))

;;; COERCE-TO-CONDITION is used in SIGNAL, ERROR, CERROR, WARN, and
;;; INVOKE-DEBUGGER for parsing the hairy argument conventions into a single
;;; argument that's directly usable by all the other routines.
;;;
(defun coerce-to-condition (datum arguments default-type function-name)
  (cond ((typep datum 'condition)
	 (if arguments
	     (cerror "Ignore the additional arguments."
		     'simple-type-error
		     :datum arguments
		     :expected-type 'null
		     :format-control "You may not supply additional arguments ~
				     when giving ~S to ~S."
		     :format-arguments (list datum function-name)))
	 datum)
        ((symbolp datum) ; Roughly, (subtypep datum 'condition).
         (apply #'make-condition datum arguments))
        ((or (stringp datum) (functionp datum))
	 (make-condition default-type
                         :format-control datum
                         :format-arguments arguments))
        (t
         (error 'simple-type-error
		:datum datum
		:expected-type '(or symbol string)
		:format-control "Bad argument to ~S: ~S"
		:format-arguments (list function-name datum)))))

#[ Exceptions

{function:catch}
{function:throw}
{function:error}
]#

(defun error (datum &rest arguments)
  "Invokes the signal facility on a condition formed from datum and arguments.
   If the condition is not handled, the debugger is invoked."
  (kernel:infinite-error-protect
    (let ((condition (coerce-to-condition datum arguments
					  'simple-error 'error))
	  (debug:*stack-top-hint* debug:*stack-top-hint*))
      (or (and (condition-function-name condition) debug:*stack-top-hint*)
	  (multiple-value-bind
	      (name frame)
	      (kernel:find-caller-name)
	    (or (condition-function-name condition)
		(setf (condition-function-name condition) name))
	    (or debug:*stack-top-hint*
		(setf debug:*stack-top-hint* frame))))
      (let ((debug:*stack-top-hint* nil))
	(signal condition))
      (invoke-debugger condition))))

;;; CERROR must take care to not use arguments when datum is already a
;;; condition object.
;;;
(defun cerror (continue-string datum &rest arguments)
  (kernel:infinite-error-protect
    (with-simple-restart
	(continue "~A" (apply #'format nil continue-string arguments))
      (let ((condition (if (typep datum 'condition)
			   datum
			   (coerce-to-condition datum arguments
						'simple-error 'error)))
	    (debug:*stack-top-hint* debug:*stack-top-hint*))
	(unless (and (condition-function-name condition)
		     debug:*stack-top-hint*)
	  (multiple-value-bind
	      (name frame)
	      (kernel:find-caller-name)
	    (unless (condition-function-name condition)
	      (setf (condition-function-name condition) name))
	    (unless debug:*stack-top-hint*
	      (setf debug:*stack-top-hint* frame))))
	(with-condition-restarts condition (list (find-restart 'continue))
	  (let ((debug:*stack-top-hint* nil))
	    (signal condition))
	  (invoke-debugger condition)))))
  nil)

(defun break (&optional (datum "Break") &rest arguments)
  "Prints a message and invokes the debugger without allowing any possibility
   of condition handling occurring."
  (system:with-screen
   (kernel:infinite-error-protect
    (with-simple-restart (continue "Return from BREAK.")
      (let ((debug:*stack-top-hint*
	     (or debug:*stack-top-hint*
		 (nth-value 1 (kernel:find-caller-name)))))
	(invoke-debugger
	 (coerce-to-condition datum arguments 'simple-condition 'break))))))
  nil)

(defun warn (datum &rest arguments)
  "Warns about a situation by signalling a condition formed by datum and
   arguments.  While the condition is being signaled, a muffle-warning restart
   exists that causes WARN to immediately return nil."
  (kernel:infinite-error-protect
    (let ((condition (coerce-to-condition datum arguments
					  'simple-warning 'warn)))
      (check-type condition warning "a warning condition")
      (restart-case (signal condition)
	(muffle-warning ()
	  :report "Skip warning."
	  (return-from warn nil)))
      (format *error-output* "~&~@<Warning:  ~3i~:_~A~:>~%" condition)))
  nil)

(in-package "LISP")


;;;; Documentation.

(defvar *documentation* (make-string-table))

(defstruct (docnode (:constructor
		     make-doc-node (content &optional file position)))
  "A documentation node."
  content    ; doc text
  file       ; file in which node is defined
  position)  ; position in file

#[ Documentation

[ Introduction      ]     A brief overview.

== User ==

[ Welcome           ]     The editor welcome message.
[ Editor Tutorial   ]     Editor introduction, for new users.
[ Editor            ]     Editor manual.
[ Applications      ]     Editor extensions: Mail, News and more.
[ Read-Eval-Print   ]     The command line interface.

== Programmer ==

[ Lisp              ]     The programming language of this system.
[ Lisp Tutorial     ]     A quick tutorial.
[ Editor Extension  ]     How to extend the editor.
[ Scripting         ]     Writing standalone scripts.
[ System Usage      ]     Lisp libraries for the programmer.
[ Compiler          ]     The compiler; styles and techniques it encourages.
[ Debugger          ]     The run-time program inspector.
[ Profiler          ]

== Builder ==

[ Installation      ]     Installing a pre-built system.
[ System Building   ]     Building and testing the system.

== Internals ==

[ Internal Design   ]     Internal system details.
[ Directory Layout  ]     Layout of source and build directories.

== Etcetera ==

[ Reference Tables  ]

[ Support           ]     Getting help from outside the system.
[ Feedback          ]     Giving feedback, reporting errors.

[ Authors           ]     Acknowledgments.
[ Future Plans      ]     TODO.
[ Copying           ]     The legal rights to this work.
]#

; single-language, n-dimensional

#[ Introduction

Nightshade is a runtime environment for a Lisp programming language.

The goal of the Nightshade project is an entirely public domain system
defined in a single language, which a single person can maintain fairly
easily.

The system is most probably entirely in the public domain already.  Most of
the [Authors] have verified that they have given up ownership rights to the
system.

Included in the system is an Emacs-like programmer's text editor, an
interpreter, a byte compiler, a native compiler, a run-time program
inspector, a profiler and higher-level libraries for tasks such as internet
connection, mail handling and document processing.  The primary human
interface to the system is the editor, which has extensions for file
management, reading mail and reading network news.

The [Welcome] node has some pointers for getting started with the editor.
]#

#[ Scripting

Scripts can be created by adding an interpreter directive as the first
line in a file, as in

    #!/usr/bin/ni
    ;;
    ;; Print a greeting.

    (princ "Hello world.")
    (terpri)

The Nightshade binary must be installed to /usr/bin/ for the example above
to work, typically by linking to the distributed binary, as in

    (symlink-file "/usr/bin/ni" "/usr/local/bin/nightshade")

The script is evaluated by the Lisp interpreter, so the code in the script
must be valid Lisp.  In particular, hashes (#) are treated as reader macros
as usual, so comments should be made with ; or #|.

Below is an example of a simple CGI script.

    #!/usr/bin/ni
    #!
    #! Simple Nightshade CGI script.

    (let ((data (with-output-to-string (stream)
                  (format stream "<HTML><BODY>")
                  (format stream "cgi.lisp at ~A" (format-time))
                  (format stream "</BODY></HTML>~%"))))
      (format t "Content-Length: ~A~%" (length data))
      (format t "Connection: close~%")
      (format t "Content-Type: text/html~%~%")
      (write-string data))
]#

#[ Reference Tables

[ ASCII         ]
[ C Precedence  ]     Operator precedence in the C programming language.
[ Numbers       ]     Decimal, hex, binary.
[ Normal Tables ]

== Bindings ==

[ Mail Bindings Wallchart    ]   Editor mail reader key bindings.
[ Netnews Bindings Wallchart ]   Editor news reader key bindings.
]#

#[ ASCII

     D   H   O   C       D   H   O   C       D   H   O   C       D   H   O   C
    ==  ==  ==  ==      ==  ==  ==  ==      ==  ==  ==  ==      ==  ==  ==  ==
     0   0   0  ^@      32  20  40          64  40 100   @      96  60 140   `
     1   1   1  ^A      33  21  41   !      65  41 101   A      97  61 141   a
     2   2   2  ^B      34  22  42   "      66  42 102   B      98  62 142   b
     3   3   3  ^C      35  23  43   #      67  43 103   C      99  63 143   c
     4   4   4  ^D      36  24  44   $      68  44 104   D     100  64 144   d
     5   5   5  ^E      37  25  45   %      69  45 105   E     101  65 145   e
     6   6   6  ^F      38  26  46   &      70  46 106   F     102  66 146   f
     7   7   7  ^G      39  27  47   '      71  47 107   G     103  67 147   g
     8   8  10  ^H      40  28  50   (      72  48 110   H     104  68 150   h
     9   9  11  ^I      41  29  51   )      73  49 111   I     105  69 151   i
    10   A  12          42  2A  52   *      74  4A 112   J     106  6A 152   j
    11   B  13  ^K      43  2B  53   +      75  4B 113   K     107  6B 153   k
    12   C  14  ^L      44  2C  54   ,      76  4C 114   L     108  6C 154   l
    13   D  15  ^M      45  2D  55   -      77  4D 115   M     109  6D 155   m
    14   E  16  ^N      46  2E  56   .      78  4E 116   N     110  6E 156   n
    15   F  17  ^O      47  2F  57   /      79  4F 117   O     111  6F 157   o
    16  10  20  ^P      48  30  60   0      80  50 120   P     112  70 160   p
    17  11  21  ^Q      49  31  61   1      81  51 121   Q     113  71 161   q
    18  12  22  ^R      50  32  62   2      82  52 122   R     114  72 162   r
    19  13  23  ^S      51  33  63   3      83  53 123   S     115  73 163   s
    20  14  24  ^T      52  34  64   4      84  54 124   T     116  74 164   t
    21  15  25  ^U      53  35  65   5      85  55 125   U     117  75 165   u
    22  16  26  ^V      54  36  66   6      86  56 126   V     118  76 166   v
    23  17  27  ^W      55  37  67   7      87  57 127   W     119  77 167   w
    24  18  30  ^X      56  38  70   8      88  58 130   X     120  78 170   x
    25  19  31  ^Y      57  39  71   9      89  59 131   Y     121  79 171   y
    26  1A  32  ^Z      58  3A  72   :      90  5A 132   Z     122  7A 172   z
    27  1B  33  ^[      59  3B  73   ;      91  5B 133   [     123  7B 173   {
    28  1C  34  ^\      60  3C  74   <      92  5C 134   \     124  7C 174   |
    29  1D  35  ^]      61  3D  75   =      93  5D 135   ]     125  7D 175   }
    30  1E  36  ^^      62  3E  76   >      94  5E 136   ^     126  7E 176   ~
    31  1F  37  ^_      63  3F  77   ?      95  5F 137   _     127  7F 177   

The editor command `ASCII' (in the "ascii" package) provides a similar table.
]#

#[ Number table

[FIX]
]#

#[ Normal Tables

== Probability Content from -oo to Z ==

      Z | 0.00   0.01   0.02   0.03   0.04   0.05   0.06   0.07   0.08   0.09
    ----+----------------------------------------------------------------------
    0.0 | 0.5000 0.5040 0.5080 0.5120 0.5160 0.5199 0.5239 0.5279 0.5319 0.5359
    0.1 | 0.5398 0.5438 0.5478 0.5517 0.5557 0.5596 0.5636 0.5675 0.5714 0.5753
    0.2 | 0.5793 0.5832 0.5871 0.5910 0.5948 0.5987 0.6026 0.6064 0.6103 0.6141
    0.3 | 0.6179 0.6217 0.6255 0.6293 0.6331 0.6368 0.6406 0.6443 0.6480 0.6517
    0.4 | 0.6554 0.6591 0.6628 0.6664 0.6700 0.6736 0.6772 0.6808 0.6844 0.6879
    0.5 | 0.6915 0.6950 0.6985 0.7019 0.7054 0.7088 0.7123 0.7157 0.7190 0.7224
    0.6 | 0.7257 0.7291 0.7324 0.7357 0.7389 0.7422 0.7454 0.7486 0.7517 0.7549
    0.7 | 0.7580 0.7611 0.7642 0.7673 0.7704 0.7734 0.7764 0.7794 0.7823 0.7852
    0.8 | 0.7881 0.7910 0.7939 0.7967 0.7995 0.8023 0.8051 0.8078 0.8106 0.8133
    0.9 | 0.8159 0.8186 0.8212 0.8238 0.8264 0.8289 0.8315 0.8340 0.8365 0.8389
    1.0 | 0.8413 0.8438 0.8461 0.8485 0.8508 0.8531 0.8554 0.8577 0.8599 0.8621
    1.1 | 0.8643 0.8665 0.8686 0.8708 0.8729 0.8749 0.8770 0.8790 0.8810 0.8830
    1.2 | 0.8849 0.8869 0.8888 0.8907 0.8925 0.8944 0.8962 0.8980 0.8997 0.9015
    1.3 | 0.9032 0.9049 0.9066 0.9082 0.9099 0.9115 0.9131 0.9147 0.9162 0.9177
    1.4 | 0.9192 0.9207 0.9222 0.9236 0.9251 0.9265 0.9279 0.9292 0.9306 0.9319
    1.5 | 0.9332 0.9345 0.9357 0.9370 0.9382 0.9394 0.9406 0.9418 0.9429 0.9441
    1.6 | 0.9452 0.9463 0.9474 0.9484 0.9495 0.9505 0.9515 0.9525 0.9535 0.9545
    1.7 | 0.9554 0.9564 0.9573 0.9582 0.9591 0.9599 0.9608 0.9616 0.9625 0.9633
    1.8 | 0.9641 0.9649 0.9656 0.9664 0.9671 0.9678 0.9686 0.9693 0.9699 0.9706
    1.9 | 0.9713 0.9719 0.9726 0.9732 0.9738 0.9744 0.9750 0.9756 0.9761 0.9767
    2.0 | 0.9772 0.9778 0.9783 0.9788 0.9793 0.9798 0.9803 0.9808 0.9812 0.9817
    2.1 | 0.9821 0.9826 0.9830 0.9834 0.9838 0.9842 0.9846 0.9850 0.9854 0.9857
    2.2 | 0.9861 0.9864 0.9868 0.9871 0.9875 0.9878 0.9881 0.9884 0.9887 0.9890
    2.3 | 0.9893 0.9896 0.9898 0.9901 0.9904 0.9906 0.9909 0.9911 0.9913 0.9916
    2.4 | 0.9918 0.9920 0.9922 0.9925 0.9927 0.9929 0.9931 0.9932 0.9934 0.9936
    2.5 | 0.9938 0.9940 0.9941 0.9943 0.9945 0.9946 0.9948 0.9949 0.9951 0.9952
    2.6 | 0.9953 0.9955 0.9956 0.9957 0.9959 0.9960 0.9961 0.9962 0.9963 0.9964
    2.7 | 0.9965 0.9966 0.9967 0.9968 0.9969 0.9970 0.9971 0.9972 0.9973 0.9974
    2.8 | 0.9974 0.9975 0.9976 0.9977 0.9977 0.9978 0.9979 0.9979 0.9980 0.9981
    2.9 | 0.9981 0.9982 0.9982 0.9983 0.9984 0.9984 0.9985 0.9985 0.9986 0.9986
    3.0 | 0.9987 0.9987 0.9987 0.9988 0.9988 0.9989 0.9989 0.9989 0.9990 0.9990

== Far Right Tail Probabilities ==

      Z  P{Z to oo} |   Z  P{Z to oo} |   Z  P{Z to oo}  |  Z    P{Z to oo}
    ----------------+-----------------+------------------+------------------
     2.0  0.02275   |  3.0 0.001350   |  4.0 0.00003167  |  5.0  2.867 E-7
     2.1  0.01786   |  3.1 0.0009676  |  4.1 0.00002066  |  5.5  1.899 E-8
     2.2  0.01390   |  3.2 0.0006871  |  4.2 0.00001335  |  6.0  9.866 E-10
     2.3  0.01072   |  3.3 0.0004834  |  4.3 0.00000854  |  6.5  4.016 E-11
     2.4  0.00820   |  3.4 0.0003369  |  4.4 0.000005413 |  7.0  1.280 E-12
     2.5  0.00621   |  3.5 0.0002326  |  4.5 0.000003398 |  7.5  3.191 E-14
     2.6  0.004661  |  3.6 0.0001591  |  4.6 0.000002112 |  8.0  6.221 E-16
     2.7  0.003467  |  3.7 0.0001078  |  4.7 0.000001300 |  8.5  9.480 E-18
     2.8  0.002555  |  3.8 0.00007235 |  4.8 7.933 E-7   |  9.0  1.129 E-19
     2.9  0.001866  |  3.9 0.00004810 |  4.9 4.792 E-7   |  9.5  1.049 E-21
]#

#[ C Precedence

expr_1  :       constant
()              |       paren_exp
*               |       MUL_PTR exp_no_list %prec UNARY
&               |       AND_ADDRESS exp_no_list %prec UNARY
+ -             |       PLUSMINUS exp_no_list %prec UNARY
!               |       NOT exp_no_list
~               |       BIT_NOT exp_no_list
++              |       INCREMENT exp_no_list
++              |       exp_no_list INCREMENT
sizeof          |       SIZEOF exp_no_list
sizeof ()       |       SIZEOF LPAREN type_name RPAREN  /* easiest to distinguish */
(cast)          |       LPAREN type_name RPAREN exp_no_list %prec UNARY
                |       SALLOC LPAREN type_name RPAREN
                |       AALLOC LPAREN type_name COMMA exp_no_list RPAREN
*               |       exp_no_list MUL_PTR exp_no_list
/ %             |       exp_no_list DIVMOD exp_no_list
+ -             |       exp_no_list PLUSMINUS exp_no_list
>> <<           |       exp_no_list SHIFT exp_no_list
==              |       exp_no_list COMPARISON exp_no_list
==              |       exp_no_list EQUALITY exp_no_list
&               |       exp_no_list AND_ADDRESS exp_no_list
^               |       exp_no_list BIT_XOR exp_no_list
|               |       exp_no_list BIT_OR exp_no_list
&&              |       exp_no_list AND exp_no_list
||              |       exp_no_list OR exp_no_list
? :             |       exp_no_list QMARK exp_no_list COLON exp_no_list
=               |       exp_no_list ASSIGN exp_no_list
=               |       exp_no_list OP_ASSIGN exp_no_list
()              |       exp_no_list LBRACKET exp_no_list RBRACKET
.  ->           |       exp_no_list ELEMENT ident       /* Note "ident": any symbol okay */
                |       DEFINED ident                           /* for #if processing */
                |       DEFINED LPAREN ident RPAREN
                |       amb_funcall
                |       SYMBOL LPAREN expr_1 RPAREN
                |       SYMBOL LPAREN exp_list RPAREN
                |       expr_1 LPAREN RPAREN %prec HIGH
                |       expr_1 LPAREN SYMBOL RPAREN
                |       expr_1 LPAREN symbol_list RPAREN
                |       expr_1 LPAREN expr_1 RPAREN
                |       expr_1 LPAREN exp_list RPAREN
                |       error %prec HIGH
                ;
]#

#[ Support

Please email matt@mundell.ukfsn.org for any help.
]#

#[ Feedback

Reports of errors and general feedback are welcome and appreciated.  Please
email them to matt@mundell.ukfsn.org.
]#

#[ Future Plans

The file etc:TODO (src/etc/TODO in the distribution) roughly lists plans
and ideas for future work.  The editor command `Edit TODO' (which is bound
to "meta-g t") brings up the TODO file.
]#

#[ Copying

As of version 1b Nightshade is most probably entirely public domain.  It is
based on CMUCL 18c.

All parts of CMUCL 18c that were explicitly marked as copyrighted have been
removed.  This includes PCL, the CLX interface, the MIT version of the loop
macro and some of the contrib directory.

Everyone who committed to the CMUCL 18c repository after the CMU public
domain release has confirmed that the additions they made are public
domain.  They are Douglas Crosher (dtc), Robert MacLachlan (ram) and Paul
Werkowski (pw).

Contributions committed on behalf of the contributor after the CMU public
domain release were recorded in the CVS logs.  The status of these
additions is:

  % confirmed public domain by contributor

    Marco Antoniotti, Julian Dolby, Peter Van Eynde, Fred Gilham, Erica
    Marsden, Mike McDonald, Timothy Miller, Tim Moore, Ken Olum, Raymond
    Toy.

  % reverted

    Pierpaolo Bernardi, Casper Dik, Pierre Mai, Juergen Weiss.

  % pending contact with the author

    Casper Dik, Marcus Krummenacker, Simon.

Every new addition to the CMUCL 18c base is public domain.  A few of these
new additions come from outside the Nightshade project; their sources are
detailed in the file src/etc/AUTHORS.
]#

#[ Internal Design

[ Directory Layout       ]  Layout of source and build directories.

[ Compiler Organisation  ]
[ Compiler Retargeting   ]
[ Run-Time System        ]
[ Virtual Machine        ]
[ Package Structure      ]  Packages relevant to the compiler
[ Compiler Glossary      ]

[ Writing System Tests   ]
]#

#[ Run-Time System

[ Type System          ]
[ Info Database        ]
[ Interpreter          ]
[ Debugger Information ]
[ Object Format        ]
[ Low-level            ]
[ Fasload File Format  ]
]#

#[ Low-level

[ Memory Management            ]
\section{Stacks and Globals}
\section{Heap Layout}
\section{Garbage Collection}
[ Interface to C and Assembler ]
[ Low-level debugging          ]
[ Core File Format             ]
]#

#[ Type System
[FIX]
]#

#[ Info Database
[FIX]
]#


#[ System Usage

[ Command Line Options                       ]

[ Default Interrupts for Lisp                ]

== Sort of part of the language ==

FIX ref to interfaces to compile, byte-compiler, interpreter

[ Packages                                   ]
[ Garbage Collection                         ]
[ The Reader                                 ]

== Libraries ==

[ Load                                       ]
[ Test Suite                                 ]  `deftest'
[ The Inspector                              ]
[ Saving a Core Image                        ]
[ Describe                                   ]
[ Running Programs from Lisp                 ]
[ Pathnames                                  ]
[ Filesystem Operations                      ]
[ Time Parsing and Formatting                ]
[ Unix Interface                             ]
[ Event Dispatching with SERVE-EVENT         ]
[ Aliens                                     ]
[ Interprocess Communication                 ]
[ Debugger Programmer Interface              ]
]#

#[ Interprocess Communication

Nightshade offers a facility for interprocess communication (IPC) on top of
using Unix system calls and the complications of that level of IPC.  There
is a simple remote-procedure-call (RPC) package build on top of TCP/IP
sockets.

[ The REMOTE Package ]
[ The WIRE Package   ]
[ Out-Of-Band Data   ]
]#


;;; %Initial-Function is called when a cold system starts up.  First we
;;; zoom down the *lisp-initialization-functions* doing things that wanted
;;; to happen at "load time."  Then we initialize the various subsystems
;;; and call the read-eval-print loop.  The top-level Read-Eval-Print loop
;;; (%top-level) is executed until someone (most likely the Quit function)
;;; throws to the tag %end-of-the-world.  We quit this way so that all
;;; outstanding cleanup forms in unwind-protects will get executed.

(proclaim '(special *lisp-initialization-functions*
		    *load-time-values*))

(eval-when (compile)
  (defmacro print-and-call (name)
    `(progn
       (%primitive print ,(symbol-name name))
       (,name))))

(defun hexstr (thing)
  (let ((addr (kernel:get-lisp-obj-address thing))
	(str (make-string 10)))
    (setf (char str 0) #\0
	  (char str 1) #\x)
    (dotimes (i 8)
      (let* ((nib (ldb (byte 4 0) addr))
	     (chr (char "0123456789abcdef" nib)))
	(declare (type (unsigned-byte 4) nib)
		 (base-char chr))
	(setf (char str (- 9 i)) chr
	      addr (ash addr -4))))
    str))

(defun %initial-function ()  ;; FIX change name to verb  initiate init -kernel
  "Spin the world."
  (%primitive print "In initial-function, and running.")
  #-gengc (setf *already-maybe-gcing* t)
  #-gengc (setf *gc-inhibit* t)
  #-gengc (setf *need-to-collect-garbage* ())
  (setf *gc-verbose* #-gengc () #+gengc ())
  (setf *before-gc-hooks* ())
  (setf *after-gc-hooks* ())
  #-gengc (setf unix::*interrupts-enabled* t)
  #-gengc (setf unix::*interrupt-pending* ())
  (setf *type-system-initialized* ())
  (setf *break-on-signals* ())
  #+gengc (setf conditions::*handler-clusters* ())

  ;; Many top-level forms call INFO, (SETF INFO).
  (print-and-call c::globaldb-init)

  ;; Set up the fdefn database.
  (print-and-call fdefn-init)

  ;; Some of the random top-level forms call Make-Array, which calls
  ;; Subtypep.
  (print-and-call typedef-init)
  (print-and-call class-init)
  (print-and-call type-init)

  (let ((funs (nreverse *lisp-initialization-functions*)))
    (%primitive print "Calling initialization functions...")
    (dolist (fun funs)
      #+() (%primitive print (hexstr fun))
      (typecase fun
	(function
	 (funcall fun))
	(cons
	 (case (car fun)
	   (:load-time-value
	    (setf (svref *load-time-values* (third fun))
		  (funcall (second fun))))
	   (:load-time-value-fixup
	    #-gengc
	    (setf (sap-ref-32 (second fun) 0)
		  (get-lisp-obj-address
		   (svref *load-time-values* (third fun))))
	    #+gengc
	    (do-load-time-value-fixup (second fun) (third fun) (fourth fun)))
	   #+(and x86 gencgc)
	   (:load-time-code-fixup
	    (vm::do-load-time-code-fixup (second fun) (third fun) (fourth fun)
					 (fifth fun)))
	   (t
	    (%primitive print
			"Bogus fixup in *lisp-initialization-functions*")
	    (%halt))))
	(t
	 (%primitive print
		     "Bogus function in *lisp-initialization-functions*")
	 (%halt))))
    (%primitive print "Done calling initialization functions."))
  (makunbound '*lisp-initialization-functions*)	; So it gets GC'ed.
  (makunbound '*load-time-values*)

  ;; Only do this after top level forms have run, 'cause thats where
  ;; deftypes are.
  (setf *type-system-initialized* t)

  (print-and-call os-init)
  (print-and-call filesys-init)

  (print-and-call reader-init)
  ;; Note: sharpm and backq not yet loaded, so this is not the final RT.
  (setf *readtable* (copy-readtable std-lisp-readtable))

  (print-and-call stream-init)
  (print-and-call loader-init)
  (print-and-call package-init)
  (print-and-call kernel::signal-init)
  (setf (alien:extern-alien "internal_errors_enabled" boolean) t)

  (set-floating-point-modes :traps '(:overflow #-x86 :underflow :invalid
					       :divide-by-zero))
  ;; This is necessary because some of the initial top level forms might
  ;; have changed the compilation policy in strange ways.
  (print-and-call c::proclaim-init)

  (print-and-call kernel::class-finalize)

  (%primitive print "Done initializing.")

  #-gengc (setf *already-maybe-gcing* ())
  #+gengc (setf *gc-verbose* ())
  (defun add-documentation (string &optional file position)
    "Add $string as a node of documentation.  The first line of $string is
     the title of the node."
    (with-input-from-string (stream string)
      (let ((title (string-trim '(#\space #\tab) (read-line stream))))
	(setf (getstring title *documentation*)
	      (make-doc-node (subseq string
				     (file-position stream))
			     file
			     position))))
    t)
  ;;; Add any documentation accumulated so far.
  ;;;
  (do ((doc *pre-doc* (cdr doc)))
      ((null doc))
    (add-documentation (caar doc) (cadar doc) (caddar doc)))
  (terpri)
  (princ "Nightshade kernel core image ")
  (princ (version))
  (princ ".")
  (terpri)
  (princ "[Current package: ")
  (princ (package-%name *package*))
  (princ "]")
  (terpri)
  (unix:unix-exit (catch '%end-of-the-world (%top-level))))

#+gengc
(defun do-load-time-value-fixup (object offset index)
  (declare (type index offset))
  (macrolet ((lose (msg)
	       `(progn
		  (%primitive print ,msg)
		  (%halt))))
    (let ((value (svref *load-time-values* index)))
      (typecase object
	(list
	 (case offset
	   (0 (setf (car object) value))
	   (1 (setf (cdr object) value))
	   (t (lose "Bogus offset in cons cell."))))
	(instance
	 (setf (%instance-ref object (- offset vm:instance-slots-offset))
	       value))
	(code-component
	 (setf (code-header-ref object offset) value))
	(simple-vector
	 (setf (svref object (- offset vm:vector-data-offset)) value))
	(t
	 (lose "Unknown kind of object for load-time-value fixup."))))))


;;;; Initialization functions:

;;; Print seems to not like x86 NPX denormal floats like
;;; least-negative-single-float, so the :underflow exceptions is disabled
;;; by default.  The user can explicitly enable them if desired.

(defun reinit ()
  (system:block-interrupts
   (without-gcing
    (os-init)
    (stream-reinit)
    (kernel::signal-init)
    (gc-init)
    (setf (alien:extern-alien "internal_errors_enabled" boolean) t)
    (set-floating-point-modes :traps
			      '(:overflow #-x86 :underflow :invalid
					  :divide-by-zero))
    ;; Clear pseudo atomic in case this core wasn't compiled with support.
    #+x86 (setf lisp::*pseudo-atomic-atomic* 0))))


;;;; Miscellaneous external functions:

;;; Quit gets us out, one way or another.

(defun quit (&optional recklessly-p)
  "Terminates the current Lisp.  Things are cleaned up unless Recklessly-P is
   true."
  (if recklessly-p
      (unix:unix-exit 0)
      (throw '%end-of-the-world 0)))

(defconstant repl-help-string
  "
This is the read-eval-print loop.  It reads a Lisp expression, evaluates
the expression, prints the result, then repeats.  Any Lisp expression can
be entered at the prompt, for example

    (use-package \"SHELL\")

makes the :shell package available in the current packages.

To exit, enter (quit).  To start the editor, enter (ed).

FIX Special symbols  * ** *** + ++ +++ -

The :shell package provides many Lisp equivalents of Unix style commands
for working in the read-eval-print loop.  These include

        (pwd)             print the working (current) directory
        (cd)              change to home:
        (cd \"/dir/\")      change to directory dir
        (ls)              list the current directory briefly
        (ls -l)           list the current directory verbosely
        (rm \"file\")       remove file
        (mv \"f\" \"g\")      move f to g
        (touch \"file\")    touch file.

The text printed as the prompt is controlled by the variable ext:*prompt*,
which can be set to a string or a function, for example

  (setq ext:*prompt* #'ext::prompt-long)

produces a prompt with the current package, user, host and directory.

FIX There's more information in the [Read-Eval-Print] documentation, which
is accessible inside the editor.
")

(defun help (&optional (stream *standard-output*))
  "Print a help message about the read-eval-print loop to $stream."
  (write-line repl-help-string stream)
  t)

#-mp ; Multi-processing version defined in multi-proc.lisp.
(defun sleep (n)
  "This function causes execution to be suspended for N seconds.  N may be
   any non-negative, non-complex number."
  (when (or (not (realp n))
	    (minusp n))
    (error "Invalid argument to SLEEP: ~S.~%~
            Must be a non-negative, non-complex number."
	   n))
  (multiple-value-bind (sec usec)
    (if (integerp n)
	(values n 0)
	(multiple-value-bind (sec frac) (truncate n)
	  (values sec(truncate frac 1e-6))))
    (unix:unix-select 0 0 0 0 sec usec))
  ())


;;;; SCRUB-CONTROL-STACK

(defconstant bytes-per-scrub-unit 2048)

;;; Scrub-control-stack.
;;;
#-x86
(defun scrub-control-stack ()
  "Zero the unused portion of the control stack so that old objects are not
   kept alive because of uninitialized stack variables."
  (declare (optimize (speed 3) (safety 0))
	   (values (unsigned-byte 20)))
  (labels
      ((scrub (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (cond ((= offset bytes-per-scrub-unit)
		(look (sap+ ptr bytes-per-scrub-unit) 0 count))
	       (t
		(setf (sap-ref-32 ptr offset) 0)
		(scrub ptr (+ offset vm:word-bytes) count))))
       (look (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (cond ((= offset bytes-per-scrub-unit)
		count)
	       ((zerop (sap-ref-32 ptr offset))
		(look ptr (+ offset vm:word-bytes) count))
	       (t
		(scrub ptr offset (+ count vm:word-bytes))))))
    (let* ((csp (sap-int (c::control-stack-pointer-sap)))
	   (initial-offset (logand csp (1- bytes-per-scrub-unit))))
      (declare (type (unsigned-byte 32) csp))
      (scrub (int-sap (- csp initial-offset))
	     (* (floor initial-offset vm:word-bytes) vm:word-bytes)
	     0))))

;;; Scrub-control-stack.
;;;
;;; On the x86 port the stack grows downwards, and to support grow on
;;; demand stacks the stack must be decreased as it is scrubbed.
;;;
#+x86
(defun scrub-control-stack ()
  "Zero the unused portion of the control stack so that old objects are not
   kept alive because of uninitialized stack variables."
  (scrub-control-stack))


#[ Read-Eval-Print

{function:quit}
{function:help}

shell package

]#

;;;; TOP-LEVEL loop.

(defvar / ()
  "Holds a list of all the values returned by the most recent top-level EVAL.")
(defvar // () "Gets the previous value of / when a new value is computed.")
(defvar /// () "Gets the previous value of // when a new value is computed.")
(defvar * () "Holds the value of the most recent top-level EVAL.")
(defvar ** () "Gets the previous value of * when a new value is computed.")
(defvar *** () "Gets the previous value of ** when a new value is computed.")
(defvar + () "Holds the value of the most recent top-level READ.")
(defvar ++ () "Gets the previous value of + when a new value is read.")
(defvar +++ () "Gets the previous value of ++ when a new value is read.")
(defvar - () "Holds the form curently being evaluated.")
(defvar *prompt* "* "
  "The top-level prompt string.  This also may be a function of no arguments
   that returns a simple-string.")
(defvar *in-top-level-catcher* ()
  "True if we are within the Top-Level-Catcher.  This is used by interrupt
   handlers to see whether it is O.K. to throw.")

(defun interactive-eval (form)
  "Evaluate FORM, returning whatever it returns but adjust ***, **, *, +++, ++,
  +, ///, //, /, and -."
  (setf - form)
  (let ((results (multiple-value-list (eval form))))
    (finish-standard-output-streams)
    (setf /// //
	  // /
	  / results
	  *** **
	  ** *
	  * (car results)))
  (setf +++ ++
	++ +
	+ -)
  (unless (boundp '*)
    ;; The bogon returned an unbound marker.
    (setf * ())
    (cerror "Go on with * set to ()."
	    "EVAL returned an unbound marker."))
  (values-list /))

(defconstant eofs-before-quit 10)

(defun %top-level ()
  "Top-level read-eval-print loop."
  (let  ((* ()) (** ()) (*** ())
	 (- ()) (+ ()) (++ ()) (+++ ())
	 (/// ()) (// ()) (/ ())
	 (magic-eof-cookie (cons :eof ()))
	 (number-of-eofs 0))
    (loop
      (with-simple-restart (abort "Return to Top-Level.")
	(catch 'top-level-catcher
	  (unix:unix-sigsetmask 0)
	  (let ((*in-top-level-catcher* t))
	    (loop
	      (scrub-control-stack)
	      (or *batch-mode*
		  (progn
		    (fresh-line)
		    (princ (if (functionp *prompt*)
			       (funcall *prompt*)
			       *prompt*))
		    (force-output)))
	      (let ((form (read *standard-input* () magic-eof-cookie)))
		(cond ((not (eq form magic-eof-cookie))
		       (let ((results
			      (multiple-value-list (interactive-eval form))))
			 (or *batch-mode*
			     (dolist (result results)
			       (fresh-line)
			       (prin1 result))))
		       (setf number-of-eofs 0))
		      ((< number-of-eofs 1)
		       (if *batch-mode*
			   (quit)
			   (let ((stream (make-synonym-stream '*terminal-io*)))
			     (setf *standard-input* stream)
			     (setf *standard-output* stream)
			     (format t "~&Received EOF on *standard-input*, ~
					switching to *terminal-io*.~%")))
		       (incf number-of-eofs))
		      ((> number-of-eofs eofs-before-quit)
		       (format t "~&Received more than ~D EOFs; Aborting.~%"
			       eofs-before-quit)
		       (quit))
		      (t
		       (incf number-of-eofs)
		       (format t "~&Received EOF.~%")))))))))))

(defun %edit-top-level ()
  "Top-level read-eval-print loop."
  (let  ((* ()) (** ()) (*** ())
	 (- ()) (+ ()) (++ ()) (+++ ())
	 (/// ()) (// ()) (/ ())
	 (magic-eof-cookie (cons :eof ()))
	 (number-of-eofs 0))
    (loop
      (with-simple-restart (abort "Return to Top-Level.")
	(catch 'top-level-catcher
	  (unix:unix-sigsetmask 0)
	  (let ((*in-top-level-catcher* t))
	    (loop
	      (scrub-control-stack)
	      (or *batch-mode*
		  (progn
		    (fresh-line)
		    (princ (if (functionp *prompt*)
			       (funcall *prompt*)
			       *prompt*))
		    (force-output)))
	      (let ((form
		     (let ((string ""))
		       (loop
			 (let ((char (read-char *standard-input* () magic-eof-cookie)))
			   (case char
			     (magic-eof-cookie)
			     ((#\newline #\return)
			      (ignore-errors
			       (let ((form (read-from-string string () magic-eof-cookie)))
				 (or (eq form magic-eof-cookie) (return form)))))
			     (#.(code-char 1)
				(princ "goto line start" *standard-output*))
			     (t
			      (setf string (concatenate 'simple-string
							string (string char))))))))))
		(cond ((not (eq form magic-eof-cookie))
		       (let ((results
			      (multiple-value-list (interactive-eval form))))
			 (or *batch-mode*
			     (dolist (result results)
			       (fresh-line)
			       (prin1 result))))
		       (setf number-of-eofs 0))
		      ((< number-of-eofs 1)
		       (if *batch-mode*
			   (quit)
			   (let ((stream (make-synonym-stream '*terminal-io*)))
			     (setf *standard-input* stream)
			     (setf *standard-output* stream)
			     (format t "~&Received EOF on *standard-input*, ~
					switching to *terminal-io*.~%")))
		       (incf number-of-eofs))
		      ((> number-of-eofs eofs-before-quit)
		       (format t "~&Received more than ~D EOFs; Aborting.~%"
			       eofs-before-quit)
		       (quit))
		      (t
		       (incf number-of-eofs)
		       (format t "~&Received EOF.~%")))))))))))


;;; %Halt  --  Interface
;;;
;;; A convenient way to get into the assembly level debugger.
;;;
(defun %halt ()
  (with-screen
   (%primitive halt)))


#[ Authors

Many people contributed to the CMU Common Lisp (CMUCL) on which Nightshade
is based.

The sections below list all known authors of all the Nightshade code and
documentation.  Thanks to them all.

The file etc:AUTHORS (src/etc/AUTHORS) provides more details of the
contributions.

== Authors noted in the CMU source and documentation ==

David Adam, Dan Aronson, David Axmark, Miles Bader, Joseph Bates, Blaine
Burks, Rick Busdiecker, Bill Chiles, David Dill, Casper Dik, Carl Ebeling,
Scott E. Fahlman, Neal Feinberg, Charles L. Forgy, Mike Garland, Joseph
Ginder, Paul Gleichauf, Dario Guise, Sean Hallgren, Steven Handerson,
Richard Harris, Jim Healy, Joerg-Cyril Hoehl, Christopher Hoover, Todd
Kaufmann, John Kolojejchick, Jim Kowalski, Dan Kuokka, Jim Large, Simon
Leinen, Sandra Loosemore, William Lott, Robert A. MacLachlan, Bill Maddox,
David B. McDonald, Tim Moore, Jim Muller, Lee Schumacher, Guy L. Steele
Jr., Dave Touretzky, Walter van Roggen, Ivan Vazquez, Skef Wholey, George
Wood, Jamie W. Zawinski and Dan Zigmond.

== Post CMU Authors (i.e. contributors to the Internet project) ==

Marco Antoniotti, Mike Clarkson, Douglas T. Crosher, Julian Dolby, Fred
Gilham, Richard Harris, Marcus Krummenacker, Eric Marsden, Makoto
Matsumoto, Mike McDonald, Timothy Miller, T. Nishimura, Ken Olum, Alexander
Petrov, Tom Russ, Sam Steingold, Raymond Toy, Peter Van Eynde, Paul F.
Werkowski and Simon XXX.

== Authors of integrated public domain code and data ==

Scott L. Burson, Stewart M. Clamen, Nachum Dershowitz, Luke Gorrie, William
Knight, Juri Pakaste, Edward M. Reingold and Thomas Russ.

== Nightshade Authors ==

Matthew Mundell
]#


#[ FIX

A reference to this node marks documentation that needs work.
]#
