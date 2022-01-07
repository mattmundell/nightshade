(IN-PACKAGE "PACKAGE")
(SETQ *META-DATA* (MAKE-HASH-TABLE :TEST #'EQUAL))
(SETF (GETHASH "feebs" *META-DATA*)
        (LISP::MAKE-PKG-INFO :NAME "FEEBS" :DOC NIL))
(SETF (GETHASH "ops" *META-DATA*) (LISP::MAKE-PKG-INFO :NAME "OPS" :DOC "OPS."))
(SETF (GETHASH "account" *META-DATA*)
        (LISP::MAKE-PKG-INFO :NAME
                             "ACCOUNT"
                             :DOC
                             "Simple bank statement parsing and summary.

Command `Summarise Account' prompts for a bank statement in comma
seperated values (CSV) format, then prompts for a section name for each
row in the statement, and finally presents a short summary of the
statement in buffer \"Account Summary\".

Bank statement format:

29/08/2003,CPT,30-96-96,11853968,READING WOKING . CD 2723 29AUG03   ,20.00,,8519.16
01/09/2003,CPT,30-96-96,11853968,READING WOKING . CD 2723 30AUG03   ,100.00,,8419.16"))
(SETF (GETHASH "hist" *META-DATA*)
        (LISP::MAKE-PKG-INFO :NAME
                             "HIST"
                             :DOC
                             "Simple textual histogram facility.

Hist is a macro of form (HIST (min max [bucket-size]) . body)

Creates a histogram with buckets of the specified size (defaults to 1),
spanning the range from Low (inclusive) to High (exclusive), with two
additional buckets to catch values below and above this range.  The body is
executed as a progn, and every call to Hist-Record within the body provides a
value for the histogram to count.  When Body exits, the histogram is printed
out and Hist returns Nil.

A simple example:
   (hist (0 10) (dotimes (i 1000) (random 10)))
This example may make the RANDOM distribution look more normal:
   (hist (0 10 2) (dotimes (i 1000) (random 10)))
This example will show you overflow buckets:
   (hist (2 12) (dotimes (i 1000) (random 15)))"))
(SETF (GETHASH "ascii" *META-DATA*)
        (LISP::MAKE-PKG-INFO :NAME "ASCII" :DOC "ASCII table printing."))
(SETF (GETHASH "analyze" *META-DATA*)
        (LISP::MAKE-PKG-INFO :NAME "ANALYSE" :DOC "Static code analysis."))
(SETF (GETHASH "csv" *META-DATA*)
        (LISP::MAKE-PKG-INFO :NAME
                             "CSV"
                             :DOC
                             "Comma Separated Value (CSV) file parser."))
(SETF (GETHASH "demos" *META-DATA*)
        (LISP::MAKE-PKG-INFO :NAME
                             "DEMOS"
                             :DOC
                             "Graphics demonstration programs.

The function DEMOS:DO-ALL-DEMOS will run through each of the demos once,
and the function DEMOS:DEMO will present a menu of all the demos."))
(SETF (GETHASH "psgraph" *META-DATA*)
        (LISP::MAKE-PKG-INFO :NAME "PSGRAPH" :DOC NIL))
