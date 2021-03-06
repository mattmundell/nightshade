;;; Stuff that knows about dumping FASL files.

(in-package "C")

(proclaim '(special compiler-version))

#[ Dumping

    Convert the compiled code into an object file or in-core function.

Phase position: 23/23 (back)

Presence: required

Files: debug-dump, dump, generic/core

Entry functions: `fasl-dump-component'

Call sequences:

    native-compile-component
      fasl-dump-component
        fasl-validate-structure
        dump-code-object
        dump-one-entry
        alter-code-object

    native-compile-component
      make-core-component
        do-core-fixups
          vm:fixup-code-object
        make-function-entry
          note-function
        vm:sanctify-for-execution
        reference-core-function


[Fasload File Format]

    So far as input to the dumper/loader, how about having a list of Entry-Info
    structures in the VMR-Component?  These structures contain all information
    needed to dump the associated function objects, and are only implicitly
    associated with the functional/XEP data structures.  Load-time constants that
    reference these function objects should specify the Entry-Info, rather than the
    functional (or something).  We would then need to maintain some sort of
    association so VMR conversion can find the appropriate Entry-Info.
    Alternatively, we could initially reference the functional, and then later
    clobber the reference to the Entry-Info.

    We have some kind of post-pass that runs after assembly, going through the
    functions and constants, annotating the VMR-Component for the benefit of the
    dumper:
	Resolve :Label load-time constants.
	Make the debug info.
	Make the entry-info structures.

    Fasl dumper and in-core loader are implementation (but not instruction set)
    dependent, so we want to give them a clear interface.

    open-fasl-file name => fasl-file
	Returns a "fasl-file" object representing all state needed by the dumper.
	We objectify the state, since the fasdumper should be reentrant.  (but
	could fail to be at first.)

    close-fasl-file fasl-file abort-p
	Close the specified fasl-file.

    fasl-dump-component component code-vector length fixups fasl-file
	Dump the code, constants, etc. for component.  Code-Vector is a vector
	holding the assembled code.  Length is the number of elements of Vector
	that are actually in use.  Fixups is a list of conses (offset . fixup)
	describing the locations and things that need to be fixed up at load time.
	If the component is a top-level component, then the top-level lambda will
	be called after the component is loaded.

    load-component component code-vector length fixups
	Like Fasl-Dump-Component, but directly installs the code in core, running
	any top-level code immediately.  (???) but we need some way to glue
	together the componenents, since we don't have a fasl table.

Dumping:

Dump code for each component after compiling that component, but defer dumping
of other stuff.  We do the fixups on the code vectors, and accumulate them in
the table.

We have to grovel the constants for each component after compiling that
component so that we can fix up load-time constants.  Load-time constants are
values needed by the code that are computed after code generation/assembly
time.  Since the code is fixed at this point, load-time constants are always
represented as non-immediate constants in the constant pool.  A load-time
constant is distinguished by being a cons (Kind . What), instead of a Constant
leaf.  Kind is a keyword indicating how the constant is computed, and What is
some context.

Some interesting load-time constants:

    (:label . <label>)
        Is replaced with the byte offset of the label within the code-vector.

    (:code-vector . <component>)
        Is replaced by the component's code-vector.

    (:entry . <function>)
    (:closure-entry . <function>)
	Is replaced by the function-entry structure for the specified function.
	:Entry is how the top-level component gets a handle on the function
	definitions so that it can set them up.

We also need to remember the starting offset for each entry, although these
don't in general appear as explicit constants.

We then dump out all the :Entry and :Closure-Entry objects, leaving any
constant-pool pointers uninitialized.  After dumping each :Entry, we dump some
stuff to let genesis know that this is a function definition.  Then we dump all
the constant pools, fixing up any constant-pool pointers in the already-dumped
function entry structures.

The debug-info *is* a constant: the first constant in every constant pool.  But
the creation of this constant must be deferred until after the component is
compiled, so we leave a (:debug-info) placeholder.
    Or maybe this is implicitly added in by the dumper, being supplied in a
    VMR-component slot.


    Work out details of the interface between the back-end and the
    assembler/dumper.

    Support for multiple assemblers concurrently loaded?  (for byte code)

    We need various mechanisms for getting information out of the assembler.

    We can get entry PCs and similar things into function objects by making a
    Constant leaf, specifying that it goes in the closure, and then
    setting the value after assembly.

    We have an operation Label-Value which can be used to get the value of a
    label after assembly and before the assembler data structures are
    deallocated.

    The function map can be constructed without any special help from the
    assembler.  Codegen just has to note the current label when the function
    changes from one block to the next, and then use the final value of these
    labels to make the function map.

    Probably we want to do the source map this way too.  Although this will
    make zillions of spurious labels, we would have to effectively do that
    anyway.

    With both the function map and the source map, getting the locations right
    for uses of Elsewhere will be a bit tricky.  Users of Elsewhere will need
    to know about how these maps are being built, since they must record the
    labels and corresponding information for the elsewhere range.  It would be
    nice to have some cooperation from Elsewhere so that this isn't necessary,
    otherwise some VOP writer will break the rules, resulting in code that is
    nowhere.

    The Debug-Info and related structures are dumped by consing up the
    structure and making it be the value of a constant.

    Getting the code vector and fixups dumped may be a bit more interesting.  I
    guess we want a Dump-Code-Vector function which dumps the code and fixups
    accumulated by the current assembly, returning a magic object that will
    become the code vector when it is dumped as a constant.
]#


;;;; Fasl dumper state.

;;; We do some buffering in front of the stream that represents the output file
;;; so as to speed things up a bit.
;;;
(defconstant fasl-buffer-size 2048)

;;; The Fasl-File structure represents everything we need to know about dumping
;;; to a fasl file.  We need to objectify the state, since the fasdumper must
;;; be reentrant.
;;;
(defstruct (fasl-file
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d) (stream stream))
	       (format stream "#<Fasl-File ~S>"
		       (namestring (fasl-file-stream s))))))
  ;;
  ;; The stream we dump to.
  (stream (required-argument) :type stream)
  ;;
  ;; The buffer we accumulate output in before blasting it out to the stream
  ;; with `sys:output-raw-bytes'.
  (buffer (make-array fasl-buffer-size :element-type '(unsigned-byte 8))
	  :type (simple-array (unsigned-byte 8) (*)))
  ;;
  ;; The index of the first free byte in Buffer.  Note that there is always at
  ;; least one byte free.
  (buffer-index 0 :type index)
  ;;
  ;; Hashtables we use to keep track of dumped constants so that we can get
  ;; them from the table rather than dumping them again.  The EQUAL-TABLE is
  ;; used for lists and strings, and the EQ-TABLE is used for everything else.
  ;; We use a separate EQ table to avoid performance patholigies with objects
  ;; for which EQUAL degnerates to EQL.  Everything entered in the EQUAL table
  ;; is also entered in the EQ table.
  (equal-table (make-hash-table :test #'equal) :type hash-table)
  (eq-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; The table's current free pointer: the next offset to be used.
  (table-free 0 :type index)
  ;;
  ;; Alist (Package . Offset) of the table offsets for each package we have
  ;; currently located.
  (packages () :type list)
  ;;
  ;; Table mapping from the Entry-Info structures for dumped XEPs to the table
  ;; offsets of the corresponding code pointers.
  (entry-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; Table holding back-patching info for forward references to XEPs.  The key
  ;; is the Entry-Info structure for the XEP, and the value is a list of conses
  ;; (<code-handle> . <offset>), where <code-handle> is the offset in the table
  ;; of the code object needing to be patched, and <offset> is the offset that
  ;; must be patched.
  (patch-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; A list of the table handles for all of the DEBUG-INFO structures dumped in
  ;; this file.  These structures must be back-patched with source location
  ;; information when the compilation is complete.
  (debug-info () :type list)
  ;;
  ;; Used to keep track of objects that we are in the process of dumping so
  ;; that circularities can be preserved.  The key is the object that we have
  ;; previously seen, and the value is the object that we reference in the
  ;; table to find this previously seen object.  (The value is never NIL.)
  ;;
  ;; Except with list objects, the key and the value are always the same.  In a
  ;; list, the key will be some tail of the value.
  (circularity-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; Hash table of structures that are allowed to be dumped.  If we try to
  ;; dump a structure that isn't in this hash table, we lose.
  (valid-structures (make-hash-table :test #'eq) :type hash-table))

;;; This structure holds information about a circularity.
;;;
(defstruct circularity
  ;;
  ;; Kind of modification to make to create circularity.
  (type (required-argument) :type (member :rplaca :rplacd :svset :struct-set))
  ;;
  ;; Object containing circularity.
  object
  ;;
  ;; Index in object for circularity.
  (index (required-argument) :type index)
  ;;
  ;; The object to be stored at Index in Object.  This is that the key that we
  ;; were using when we discovered the circularity.
  value
  ;;
  ;; The value that was associated with Value in the CIRCULARITY-TABLE.  This
  ;; is the object that we look up in the EQ-TABLE to locate Value.
  enclosing-object)

;;; A list of the Circularity structures for all of the circularities detected
;;; in the current top-level call to Dump-Object.  Setting this lobotomizes
;;; circularity detection as well, since circular dumping uses the table.
;;;
(defvar *circularities-detected*)

;;; Used to inhibit table access when dumping forms to be read by the cold
;;; loader.
;;;
(defvar *cold-load-dump* nil)

;;; Used to turn off the structure validation during dumping of source info.
;;;
(defvar *dump-only-valid-structures* t)


;;;; Utilities.

;;; FLUSH-FASL-FILE-BUFFER  --  Internal
;;;
;;; Write out the contents of File's buffer to its stream.
;;;
(defun flush-fasl-file-buffer (file)
  (system:output-raw-bytes (fasl-file-stream file)
			   (fasl-file-buffer file)
			   0
			   (fasl-file-buffer-index file))
  (setf (fasl-file-buffer-index file) 0)
  (undefined-value))

;;; Dump-Byte  --  Internal
;;;
;;; Write the byte B to the specified fasl-file stream.
;;;
(declaim (maybe-inline dump-byte))
(defun dump-byte (b file)
  (declare (type (unsigned-byte 8) b) (type fasl-file file)
	   (optimize (speed 3) (safety 0)))
  (let ((idx (fasl-file-buffer-index file))
	(buf (fasl-file-buffer file)))
    (setf (aref buf idx) b)
    (let ((new (1+ idx)))
      (setf (fasl-file-buffer-index file) new)
      (when (= new fasl-buffer-size)
	(flush-fasl-file-buffer file))))
  (undefined-value))

;;; DUMP-UNSIGNED-32  --  Internal
;;;
;;; Dump a 4 byte unsigned integer.
;;;
(defun dump-unsigned-32 (num file)
  (declare (type (unsigned-byte 32) num) (type fasl-file file)
	   (optimize (speed 3) (safety 0)))
  (let* ((idx (fasl-file-buffer-index file))
	 (buf (fasl-file-buffer file))
	 (new (+ idx 4)))
    (when (>= new fasl-buffer-size)
      (flush-fasl-file-buffer file)
      (setq idx 0  new 4))
    (setf (aref buf (+ idx 0)) (ldb (byte 8 0) num))
    (setf (aref buf (+ idx 1)) (ldb (byte 8 8) num))
    (setf (aref buf (+ idx 2)) (ldb (byte 8 16) num))
    (setf (aref buf (+ idx 3)) (ldb (byte 8 24) num))
    (setf (fasl-file-buffer-index file) new))
  (undefined-value))

;;; Dump-Var-Signed   --  Internal
;;;
;;; Dump Num to the fasl stream, represented by the specified number of
;;; bytes.
;;;
(defun dump-var-signed  (num bytes file)
  (declare (integer num) (type index bytes) (type fasl-file file)
	   (inline dump-byte))
  (do ((n num (ash n -8))
       (i bytes (1- i)))
      ((= i 0))
    (declare (type index i))
    (dump-byte (logand n #xFF) file))
  (undefined-value))

;;; DUMP-BYTES  --  Internal
;;;
;;; Dump the first N bytes in Vec out to File.  Vec is some sort of unboxed
;;; vector-like thing that we can BLT from.
;;;
(defun dump-bytes (vec n file)
  (declare (type index n) (type fasl-file file)
	   (optimize (speed 3) (safety 0)))
  (let* ((idx (fasl-file-buffer-index file))
	 (buf (fasl-file-buffer file))
	 (new (+ idx n)))
    (cond ((< new fasl-buffer-size)
	   (bit-bash-copy vec vector-data-bit-offset
			  buf
			  (+ vector-data-bit-offset
			     (the index (* idx vm:byte-bits)))
			  (* n vm:byte-bits))
	   (setf (fasl-file-buffer-index file) new))
	  (t
	   (flush-fasl-file-buffer file)
	   (cond ((>= n fasl-buffer-size)
		  (system:output-raw-bytes (fasl-file-stream file)
					   vec 0 n))
		 (t
		  (bit-bash-copy vec vector-data-bit-offset
				 buf vector-data-bit-offset
				 (* n vm:byte-bits))
		  (setf (fasl-file-buffer-index file) n))))))
  (undefined-value))

;;; Dump-FOP  --  Internal
;;;
;;; Dump the FOP code for the named FOP to the specified fasl-file.
;;;
(defmacro dump-fop (fs file)
  (let* ((fs (eval fs))
	 (val (get fs 'lisp::fop-code)))
    (assert val () "Compiler bug: ~S not a legal fasload operator." fs)
    `(dump-byte ',val ,file)))

;;; Dump-FOP*  --  Internal
;;;
;;; Dump a FOP-Code along with an integer argument, choosing the FOP based
;;; on whether the argument will fit in a single byte.
;;;
(defmacro dump-fop* (n byte-fop word-fop file)
  (once-only ((n-n n)
	      (n-file file))
    `(cond ((< ,n-n 256)
	    (dump-fop ',byte-fop ,n-file)
	    (dump-byte ,n-n ,n-file))
	   (t
	    (dump-fop ',word-fop ,n-file)
	    (dump-unsigned-32 ,n-n ,n-file)))))

;;; Dump-Push  --  Internal
;;;
;;; Push the object at table offset Handle on the fasl stack.
;;;
(defun dump-push (handle file)
  (declare (type index handle) (type fasl-file file))
  (dump-fop* handle lisp::fop-byte-push lisp::fop-push file)
  (undefined-value))

;;; Dump-Pop  --  Internal
;;;
;;; Pop the object currently on the fasl stack top into the table, and
;;; return the table index, incrementing the free pointer.
;;;
(defun dump-pop (file)
  (prog1 (fasl-file-table-free file)
    (dump-fop 'lisp::fop-pop file)
    (incf (fasl-file-table-free file))))

;;; EQUAL-CHECK-TABLE  --  Internal
;;;
;;; If X is in File's EQUAL-TABLE, then push the object and return T,
;;; otherwise NIL.  If *COLD-LOAD-DUMP* is true, then do nothing and return
;;; NIL.
;;;
(defun equal-check-table (x file)
  (declare (type fasl-file file))
  (unless *cold-load-dump*
    (let ((handle (gethash x (fasl-file-equal-table file))))
      (cond (handle
	     (dump-push handle file)
	     t)
	    (t
	     nil)))))

;;; EQ-SAVE-OBJECT, EQUAL-SAVE-OBJECT  --  Internal
;;;
;;; These functions are called after dumping an object to save the object
;;; in the table.  The object (also passed in as X) must already be on the
;;; top of the FOP stack.  If *COLD-LOAD-DUMP* is true, then we don't do
;;; anything.
;;;
(defun eq-save-object (x file)
  (declare (type fasl-file file))
  (unless *cold-load-dump*
    (let ((handle (dump-pop file)))
      (setf (gethash x (fasl-file-eq-table file)) handle)
      (dump-push handle file)))
  (undefined-value))
;;;
(defun equal-save-object (x file)
  (declare (type fasl-file file))
  (unless *cold-load-dump*
    (let ((handle (dump-pop file)))
      (setf (gethash x (fasl-file-equal-table file)) handle)
      (setf (gethash x (fasl-file-eq-table file)) handle)
      (dump-push handle file)))
  (undefined-value))

;;; NOTE-POTENTIAL-CIRCULARITY  --  Internal
;;;
;;; Record X in File's CIRCULARITY-TABLE unless *COLD-LOAD-DUMP* is true.
;;; This is called on objects that we are about to dump might have a
;;; circular path through them.
;;;
;;; The object must not currently be in this table, since the dumper should
;;; never be recursively called on a circular reference.  Instead, the dumping
;;; function must detect the circularity and arrange for the dumped object to
;;; be patched.
;;;
(defun note-potential-circularity (x file)
  (unless *cold-load-dump*
    (let ((circ (fasl-file-circularity-table file)))
      (assert (not (gethash x circ)))
      (setf (gethash x circ) x)))
  (undefined-value))

;;; Fasl-Dump-Cold-Load-Form  --  Interface
;;;
;;; Dump Form to a fasl file so that it evaluated at load time in normal
;;; load and at cold-load time in cold load.  This is used to dump package
;;; frobbing forms.
;;;
(defun fasl-dump-cold-load-form (form file)
  (declare (type fasl-file file))
  (dump-fop 'lisp::fop-normal-load file)
  (let ((*cold-load-dump* t))
    (dump-object form file))
  (dump-fop 'lisp::fop-eval-for-effect file)
  (dump-fop 'lisp::fop-maybe-cold-load file)
  (undefined-value))


;;;; Opening and closing.

;;; Open-Fasl-File  --  Interface
;;;
;;; Return a Fasl-File object for dumping to the named file.  Some
;;; information about the source is specified by the string Where.  If
;;; byte-p is true, this file will contain no native code, and is thus
;;; largely implementation independent.
;;;
(defun open-fasl-file (name where &optional byte-p)
  (declare (type pathname name))
  (let* ((stream (open name :direction :output
		       :if-exists :new-version
		       :element-type '(unsigned-byte 8)))
	 (res (make-fasl-file :stream stream)))
    (multiple-value-bind
	(version f-vers f-imp)
	(if byte-p
	    (values "Byte code"
		    byte-fasl-file-version
		    (backend-byte-fasl-file-implementation *backend*))
	    (values (backend-version *backend*)
		    (backend-fasl-file-version *backend*)
		    (backend-fasl-file-implementation *backend*)))
      (format stream
	      "FASL FILE output from ~A.~@
	       Compiled ~A on ~A~@
	       Compiler ~A, Lisp ~A~@
	       Targeted for ~A, FASL version ~D~%"
	      where
	      (ext:format-universal-time nil (get-universal-time))
	      (machine-instance) compiler-version
	      (version)
	      version f-vers)
      ;;
      ;; Terminate header.
      (dump-byte 255 res)
      ;;
      ;; Specify code format.
      (dump-fop 'lisp::fop-code-format res)
      (dump-byte f-imp res)
      (dump-byte f-vers res))
    res))

;;; Close-Fasl-File  --  Interface
;;;
;;; Close the specified Fasl-File, aborting the write if Abort-P is true.
;;; We do various sanity checks, then end the group.
;;;
(defun close-fasl-file (file abort-p)
  (declare (type fasl-file file))
  (assert (zerop (hash-table-count (fasl-file-patch-table file))))
  (dump-fop 'lisp::fop-verify-empty-stack file)
  (dump-fop 'lisp::fop-verify-table-size file)
  (dump-unsigned-32 (fasl-file-table-free file) file)
  (dump-fop 'lisp::fop-end-group file)
  (flush-fasl-file-buffer file)
  (close (fasl-file-stream file) :abort abort-p)
  (undefined-value))


;;;; Component (function) dumping.

(defun dump-segment (segment code-length file)
  (declare (type new-assem:segment segment)
	   (type fasl-file file))
  (flush-fasl-file-buffer file)
  (let* ((stream (fasl-file-stream file))
	 (posn (file-position stream)))
    (new-assem:segment-map-output
     segment
     #'(lambda (sap amount)
	 (system:output-raw-bytes stream sap 0 amount)))
    (unless (= (- (file-position stream) posn) code-length)
      (error "Tried to output ~D bytes, but only ~D made it."
	     code-length (- (file-position stream) posn))))
  (when (backend-featurep :gengc)
    (unless (zerop (logand code-length 3))
      (dotimes (i (- 4 (logand code-length 3)))
	(dump-byte 0 file))))
  (undefined-value))

;;; Dump-Code-Object  --  Internal
;;;
;;; Dump out the constant pool and code-vector for component, push the
;;; result in the table and return the offset.
;;;
;;; The only tricky thing is handling constant-pool references to
;;; functions.  If we have already dumped the function, then we just push
;;; the code pointer.  Otherwise, we must create back-patching information
;;; so that the constant will be set when the function is eventually
;;; dumped.  This is a bit awkward, since we don't have the handle for the
;;; code object being dumped while we are dumping its constants.
;;;
;;; We dump trap objects in any unused slots or forward referenced slots.
;;;
(defun dump-code-object (component code-segment code-length
				   trace-table fixups file)
  (declare (type component component) (type fasl-file file)
	   (list trace-table) (type index code-length))
  (let* ((2comp (component-info component))
	 (constants (ir2-component-constants 2comp))
	 (gengc (backend-featurep :gengc))
	 (header-length (length constants))
	 (trace-table (pack-trace-table trace-table))
	 (trace-table-length (length trace-table))
	 (total-length (+ code-length (* trace-table-length 2))))
    (collect ((patches))
      ;; Dump the debug info.
      (when gengc
	(let ((info (debug-info-for-component component))
	      (*dump-only-valid-structures* nil))
	  (dump-object info file)
	  (let ((info-handle (dump-pop file)))
	    (dump-push info-handle file)
	    (push info-handle (fasl-file-debug-info file)))))

      ;; Dump the offset of the trace table.
      (dump-object code-length file)

      ;; Dump the constants, noting any :entries that have to be fixed up.
      (do ((i vm:code-constants-offset (1+ i)))
	  ((>= i header-length))
	(let ((entry (aref constants i)))
	  (etypecase entry
	    (constant
	     (dump-object (constant-value entry) file))
	    (cons
	     (ecase (car entry)
	       (:entry
		(let* ((info (leaf-info (cdr entry)))
		       (handle (gethash info (fasl-file-entry-table file))))
		  (cond
		   (handle
		    (dump-push handle file))
		   (t
		    (patches (cons info i))
		    (dump-fop 'lisp::fop-misc-trap file)))))
	       (:load-time-value
		(dump-push (cdr entry) file))
	       (:fdefinition
		(dump-object (cdr entry) file)
		(dump-fop 'lisp::fop-fdefinition file))))
	    (null
	     (dump-fop 'lisp::fop-misc-trap file)))))

      ;; Dump the debug info.
      (unless gengc
	(let ((info (debug-info-for-component component))
	      (*dump-only-valid-structures* nil))
	  (dump-object info file)
	  (let ((info-handle (dump-pop file)))
	    (dump-push info-handle file)
	    (push info-handle (fasl-file-debug-info file)))))

      (let ((num-consts (if gengc
			    (- header-length vm:code-debug-info-slot)
			    (- header-length vm:code-trace-table-offset-slot)))
	    (total-length (if gengc
			      (ceiling total-length 4)
			      total-length)))
	(cond ((and (< num-consts #x100) (< total-length #x10000))
	       (dump-fop 'lisp::fop-small-code file)
	       (dump-byte num-consts file)
	       (dump-var-signed total-length 2 file))
	      (t
	       (dump-fop 'lisp::fop-code file)
	       (dump-unsigned-32 num-consts file)
	       (dump-unsigned-32 total-length file))))

      (dump-segment code-segment code-length file)
      (dump-i-vector trace-table file t)
      (dump-fixups fixups file)
      (dump-fop 'lisp::fop-sanctify-for-execution file)
      (let ((handle (dump-pop file)))
	(dolist (patch (patches))
	  (push (cons handle (cdr patch))
		(gethash (car patch) (fasl-file-patch-table file))))
	handle))))

(defun dump-assembler-routines (code-segment length fixups routines file)
  (dump-fop 'lisp::fop-assembler-code file)
  (dump-unsigned-32 (if (backend-featurep :gengc)
			(ceiling length 4)
			length)
		    file)
  (flush-fasl-file-buffer file)
  (let ((stream (fasl-file-stream file)))
    (new-assem:segment-map-output
     code-segment
     #'(lambda (sap amount)
	 (system:output-raw-bytes stream sap 0 amount))))
  (dolist (routine routines)
    (dump-fop 'lisp::fop-normal-load file)
    (let ((*cold-load-dump* t))
      (dump-object (car routine) file))
    (dump-fop 'lisp::fop-maybe-cold-load file)
    (dump-fop 'lisp::fop-assembler-routine file)
    (dump-unsigned-32 (label-position (cdr routine)) file))
  (dump-fixups fixups file)
  (dump-fop 'lisp::fop-sanctify-for-execution file)
  (dump-pop file))

;;; Dump-Fixups  --  Internal
;;;
;;; Dump all the fixups.  Currently there are three flavors of fixup:
;;;  - assembly routines: named by a symbol
;;;  - foreign (C) symbols: named by a string
;;;  - code object references: don't need a name.
;;;
(defun dump-fixups (fixups file)
  (declare (list fixups) (type fasl-file file))
  (dolist (info fixups)
    (let* ((kind (first info))
	   (fixup (second info))
	   (name (fixup-name fixup))
	   (flavor (fixup-flavor fixup))
	   (offset (third info)))
      (dump-fop 'lisp::fop-normal-load file)
      (let ((*cold-load-dump* t))
	(dump-object kind file))
      (dump-fop 'lisp::fop-maybe-cold-load file)
      (ecase flavor
	(:assembly-routine
	 (assert (symbolp name))
	 (dump-fop 'lisp::fop-normal-load file)
	 (let ((*cold-load-dump* t))
	   (dump-object name file))
	 (dump-fop 'lisp::fop-maybe-cold-load file)
	 (dump-fop 'lisp::fop-assembler-fixup file))
	(:foreign
	 (assert (stringp name))
	 (dump-fop 'lisp::fop-foreign-fixup file)
	 (let ((len (length name)))
	   (assert (< len 256))
	   (dump-byte len file)
	   (dotimes (i len)
	     (dump-byte (char-code (schar name i)) file))))
	(:code-object
	 (dump-fop 'lisp::fop-code-object-fixup file)))
      (dump-unsigned-32 offset file)))
  (undefined-value))

;;; Dump-One-Entry  --  Internal
;;;
;;; Dump a function-entry data structure corresponding to Entry to File.
;;; Code-Handle is the table offset of the code object for the component.
;;;
;;; If the entry is a DEFUN, then we also dump a FOP-FSET so that the cold
;;; loader can instantiate the definition at cold-load time, allowing forward
;;; references to functions in top-level forms.
;;;
(defun dump-one-entry (entry code-handle file)
  (declare (type entry-info entry) (type index code-handle)
	   (type fasl-file file))
  (let ((name (entry-info-name entry)))
    (dump-push code-handle file)
    (dump-object name file)
    (dump-object (entry-info-arguments entry) file)
    (dump-object (entry-info-type entry) file)
    (dump-fop 'lisp::fop-function-entry file)
    (dump-unsigned-32 (label-position (entry-info-offset entry)) file)
    (let ((handle (dump-pop file)))
      (when (and name (or (symbolp name) (listp name)))
	(dump-object name file)
	(dump-push handle file)
	(dump-fop 'lisp::fop-fset file))
      handle)))

;;; Alter-Code-Object  --  Internal
;;;
;;; Alter the code object referenced by Code-Handle at the specified
;;; Offset, storing the object referenced by Entry-Handle.
;;;
(defun alter-code-object (code-handle offset entry-handle file)
  (declare (type index code-handle entry-handle offset) (type fasl-file file))
  (dump-push code-handle file)
  (dump-push entry-handle file)
  (dump-fop* offset lisp::fop-byte-alter-code lisp::fop-alter-code file)
  (undefined-value))

;;; Fasl-Dump-Component  --  Interface
;;;
;;; Dump the code, constants, etc. for component.  We pass in the assembler
;;; fixups, code vector and node info.
;;;
(defun fasl-dump-component (component code-segment length trace-table
				      fixups file)
  (declare (type component component) (list trace-table) (type fasl-file file))

  (dump-fop 'lisp::fop-verify-empty-stack file)
  (dump-fop 'lisp::fop-verify-table-size file)
  (dump-unsigned-32 (fasl-file-table-free file) file)

  (let ((info (ir2-component-dyncount-info (component-info component))))
    (when info
      (fasl-validate-structure info file)))

  (let ((code-handle (dump-code-object component code-segment
				       length trace-table fixups file))
	(2comp (component-info component)))
    (dump-fop 'lisp::fop-verify-empty-stack file)

    (dolist (entry (ir2-component-entries 2comp))
      (let ((entry-handle (dump-one-entry entry code-handle file)))
	(setf (gethash entry (fasl-file-entry-table file)) entry-handle)

	(let ((old (gethash entry (fasl-file-patch-table file))))
	  (when old
	    (dolist (patch old)
	      (alter-code-object (car patch) (cdr patch) entry-handle file))
	    (remhash entry (fasl-file-patch-table file)))))))
  (undefined-value))

;;; DUMP-BYTE-CODE-OBJECT -- internal.
;;;
(defun dump-byte-code-object (segment length constants file)
  (declare (type new-assem:segment segment)
	   (type index length)
	   (type vector constants)
	   (type fasl-file file))
  (let ((gengc (backend-featurep :gengc)))
    (collect ((entry-patches))
      ;; Dump the debug info.
      (when gengc
	(let ((info (make-debug-info :name
				     (component-name *compile-component*)))
	      (*dump-only-valid-structures* nil))
	  (dump-object info file)
	  (let ((info-handle (dump-pop file)))
	    (dump-push info-handle file)
	    (push info-handle (fasl-file-debug-info file)))))

      ;; "trace table" is initialized by loader to hold a list of all byte
      ;; functions in this code object (for debug info.)
      (dump-object nil file)

      ;; Dump the constants.
      (dotimes (i (length constants))
	(let ((entry (aref constants i)))
	  (etypecase entry
	    (constant
	     (dump-object (constant-value entry) file))
	    (null
	     (dump-fop 'lisp::fop-misc-trap file))
	    (list
	     (ecase (car entry)
	       (:entry
		(let* ((info (leaf-info (cdr entry)))
		       (handle (gethash info (fasl-file-entry-table file))))
		  (cond
		   (handle
		    (dump-push handle file))
		   (t
		    (entry-patches (cons info (+ i vm:code-constants-offset)))
		    (dump-fop 'lisp::fop-misc-trap file)))))
	       (:load-time-value
		(dump-push (cdr entry) file))
	       (:fdefinition
		(dump-object (cdr entry) file)
		(dump-fop 'lisp::fop-fdefinition file))
	       (:type-predicate
		(dump-object 'load-type-predicate file)
		(let ((*unparse-function-type-simplify* t))
		  (dump-object (type-specifier (cdr entry)) file))
		(dump-fop 'lisp::fop-funcall file)
		(dump-byte 1 file)))))))

      ;; Dump the debug info.
      (unless gengc
	(let ((info (make-debug-info :name
				     (component-name *compile-component*)))
	      (*dump-only-valid-structures* nil))
	  (dump-object info file)
	  (let ((info-handle (dump-pop file)))
	    (dump-push info-handle file)
	    (push info-handle (fasl-file-debug-info file)))))

      (let ((num-consts (if gengc
			    (+ (length constants) 2)
			    (1+ (length constants))))
	    (length (if gengc
			(ceiling length 4)
			length)))
	(cond ((and (< num-consts #x100) (< length #x10000))
	       (dump-fop 'lisp::fop-small-code file)
	       (dump-byte num-consts file)
	       (dump-var-signed length 2 file))
	      (t
	       (dump-fop 'lisp::fop-code file)
	       (dump-unsigned-32 num-consts file)
	       (dump-unsigned-32 length file))))

      (dump-segment segment length file)
      (let ((code-handle (dump-pop file))
	    (patch-table (fasl-file-patch-table file)))
	(dolist (patch (entry-patches))
	  (push (cons code-handle (cdr patch))
		(gethash (car patch) patch-table)))
	code-handle))))

;;; DUMP-BYTE-FUNCTION  --  Internal
;;;
;;; Dump a BYTE-FUNCTION object.  We dump the layout and
;;; funcallable-instance info, but rely on the loader setting up the
;;; correct funcallable-instance-function.
;;;
(defun dump-byte-function (xep code-handle file)
  (let ((nslots (- (get-closure-length xep)
		   ;; 1- for header
		   (1- vm:funcallable-instance-info-offset))))
    (dotimes (i nslots)
      (if (zerop i)
	  (dump-push code-handle file)
	  (dump-object (%funcallable-instance-info xep i) file)))
    (dump-object (%funcallable-instance-layout xep) file)
    (dump-fop 'lisp::fop-make-byte-compiled-function file)
    (dump-byte nslots file))
  (undefined-value))

;;; FASL-DUMP-BYTE-COMPONENT  --  Interface
;;;
;;; Dump a byte-component.  This is similar to FASL-DUMP-COMPONENT, but
;;; different.
;;;
(defun fasl-dump-byte-component (segment length constants xeps file)
  (declare (type new-assem:segment segment)
	   (type index length)
	   (type vector constants)
	   (type list xeps)
	   (type fasl-file file))

  (let ((code-handle (dump-byte-code-object segment length constants file)))
    (dolist (noise xeps)
      (let* ((lambda (car noise))
	     (info (lambda-info lambda))
	     (xep (cdr noise)))
	(dump-byte-function xep code-handle file)
	(let* ((entry-handle (dump-pop file))
	       (patch-table (fasl-file-patch-table file))
	       (old (gethash info patch-table)))
	  (setf (gethash info (fasl-file-entry-table file)) entry-handle)
	  (when old
	    (dolist (patch old)
	      (alter-code-object (car patch) (cdr patch)
				 entry-handle file))
	    (remhash info patch-table))))))
  (undefined-value))

;;; FASL-DUMP-TOP-LEVEL-LAMBDA-CALL  --  Interface
;;;
;;; Dump a FOP-FUNCALL to call an already dumped top-level lambda at load
;;; time.
;;;
(defun fasl-dump-top-level-lambda-call (fun file)
  (declare (type clambda fun) (type fasl-file file))
  (let ((handle (gethash (leaf-info fun) (fasl-file-entry-table file))))
    (assert handle)
    (dump-push handle file)
    (dump-fop 'lisp::fop-funcall-for-effect file)
    (dump-byte 0 file))
  (undefined-value))

;;; FASL-DUMP-SOURCE-INFO  --  Interface
;;;
;;; Compute the correct list of DEBUG-SOURCE structures and backpatch all
;;; of the dumped DEBUG-INFO structures.  We clear the
;;; FASL-FILE-DEBUG-INFO, so that subsequent components with different
;;; source info may be dumped.
;;;
(defun fasl-dump-source-info (info file)
  (declare (type source-info info) (type fasl-file file))
  (let ((res (debug-source-for-info info))
	*dump-only-valid-structures*)
    (dump-object res file)
    (let ((res-handle (dump-pop file)))
      (dolist (info-handle (fasl-file-debug-info file))
	(dump-push res-handle file)
	(dump-fop 'lisp::fop-structset file)
	(dump-unsigned-32 info-handle file)
	(dump-unsigned-32 2 file))))

  (setf (fasl-file-debug-info file) ())
  (undefined-value))


;;;; Main entries to object dumping.

;;; Dump-Non-Immediate-Object  --  Internal
;;;
;;; This function deals with dumping objects that are complex enough so
;;; that we want to cache them in the table, rather than repeatedly dumping
;;; them.  If the object is in the EQ-TABLE, then we push it, otherwise, we
;;; do a type dispatch to a type specific dumping function.  The type
;;; specific branches do any appropriate EQUAL-TABLE check and table entry.
;;;
;;; When we go to dump the object, we enter it in the CIRCULARITY-TABLE.
;;;
(defun dump-non-immediate-object (x file)
  (let ((index (gethash x (fasl-file-eq-table file))))
    (cond ((and index (not *cold-load-dump*))
	   (dump-push index file))
	  (t
	   (typecase x
	     (symbol (dump-symbol x file))
	     (list
	      (cond (*coalesce-constants*
		     (unless (equal-check-table x file)
			     (dump-list x file)
			     (equal-save-object x file)))
		    (t
		     (dump-list x file)
		     (eq-save-object x file))))
	     (layout
	      (dump-layout x file)
	      (eq-save-object x file))
	     (instance
	      (dump-structure x file)
	      (eq-save-object x file))
	     (array
	      (dump-array x file))
	     (number
	      (unless (equal-check-table x file)
		(etypecase x
		  (ratio (dump-ratio x file))
		  (complex (dump-complex x file))
		  (float (dump-float x file))
		  (integer (dump-integer x file)))
		(equal-save-object x file)))
	     (t
	      ;;
	      ;; This probably never happens, since bad things are detected
	      ;; during IR1 conversion.
	      (error "This object cannot be dumped into a fasl file:~% ~S"
		     x))))))
  (undefined-value))

;;; Sub-Dump-Object  --  Internal
;;;
;;; Dump an object of any type by dispatching to the correct type-specific
;;; dumping function.  We pick off immediate objects, symbols and and magic
;;; lists here.  Other objects are handled by Dump-Non-Immediate-Object.
;;;
;;; This is the function used for recursive calls to the fasl dumper.  We don't
;;; worry about creating circularities here, since it is assumed that there is
;;; a top-level call to Dump-Object.
;;;
(defun sub-dump-object (x file)
  (cond ((listp x)
	 (if x
	     (dump-non-immediate-object x file)
	     (dump-fop 'lisp::fop-empty-list file)))
	((symbolp x)
	 (if (eq x t)
	     (dump-fop 'lisp::fop-truth file)
	     (dump-non-immediate-object x file)))
	((fixnump x) (dump-integer x file))
	((characterp x) (dump-character x file))
	(t
	 (dump-non-immediate-object x file))))

;;; Dump-Circularities  --  Internal
;;;
;;; Dump stuff to backpatch already dumped objects.  Infos is the list of
;;; Circularity structures describing what to do.  The patching FOPs take
;;; the value to store on the stack.  We compute this value by fetching the
;;; enclosing object from the table, and then CDR'ing it if necessary.
;;;
(defun dump-circularities (infos file)
  (let ((table (fasl-file-eq-table file)))
    (dolist (info infos)
      (let* ((value (circularity-value info))
	     (enclosing (circularity-enclosing-object info)))
	(dump-push (gethash enclosing table) file)
	(unless (eq enclosing value)
	  (do ((current enclosing (cdr current))
	       (i 0 (1+ i)))
	      ((eq current value)
	       (dump-fop 'lisp::fop-nthcdr file)
	       (dump-unsigned-32 i file))
	    (declare (type index i)))))

      (ecase (circularity-type info)
	(:rplaca (dump-fop 'lisp::fop-rplaca file))
	(:rplacd (dump-fop 'lisp::fop-rplacd file))
	(:svset (dump-fop 'lisp::fop-svset file))
	(:struct-set (dump-fop 'lisp::fop-structset file)))
      (dump-unsigned-32 (gethash (circularity-object info) table) file)
      (dump-unsigned-32 (circularity-index info) file))))

;;; Dump-Object  --  Interface
;;;
;;; Set up stuff for circularity detection, then dump an object.  All
;;; shared and circular structure will be exactly preserved within a single
;;; call to Dump-Object.  Sharing between objects dumped by separate calls
;;; is only preserved when convenient.
;;;
;;; We peek at the object type so that we only pay the circular detection
;;; overhead on types of objects that might be circular.
;;;
(defun dump-object (x file)
  (if (or (array-header-p x) (simple-vector-p x) (consp x) (%instancep x))
      (let ((*circularities-detected* ())
	    (circ (fasl-file-circularity-table file)))
	(clrhash circ)
	(sub-dump-object x file)
	(when *circularities-detected*
	  (dump-circularities *circularities-detected* file)
	  (clrhash circ)))
      (sub-dump-object x file)))


;;;; Load-time-value and make-load-form support.

;;; FASL-DUMP-LOAD-TIME-VALUE-LAMBDA -- interface.
;;;
;;; Emit a funcall of the function and return the handle for the result.
;;;
(defun fasl-dump-load-time-value-lambda (fun file)
  (declare (type clambda fun) (type fasl-file file))
  (let ((handle (gethash (leaf-info fun) (fasl-file-entry-table file))))
    (assert handle)
    (dump-push handle file)
    (dump-fop 'lisp::fop-funcall file)
    (dump-byte 0 file))
  (dump-pop file))

;;; FASL-CONSTANT-ALREADY-DUMPED -- interface.
;;;
;;; Return T iff CONSTANT has not already been dumped.  It's been dumped
;;; if it's in the EQ table.
;;;
(defun fasl-constant-already-dumped (constant file)
  (if (or (gethash constant (fasl-file-eq-table file))
	  (gethash constant (fasl-file-valid-structures file)))
      t
      nil))

;;; FASL-NOTE-HANDLE-FOR-CONSTANT -- interface.
;;;
;;; Use HANDLE whenever we try to dump CONSTANT.  HANDLE should have been
;;; returned earlier by FASL-DUMP-LOAD-TIME-VALUE-LAMBDA.
;;;
(defun fasl-note-handle-for-constant (constant handle file)
  (let ((table (fasl-file-eq-table file)))
    (when (gethash constant table)
      (error "~S already dumped?" constant))
    (setf (gethash constant table) handle))
  (undefined-value))

;;; FASL-VALIDATE-STRUCTURE -- interface.
;;;
;;; Note that the specified structure can just be dumped by enumerating the
;;; slots.
;;;
(defun fasl-validate-structure (structure file)
  (setf (gethash structure (fasl-file-valid-structures file)) t)
  (undefined-value))


;;;; Number Dumping.

;;; Dump a ratio

(defun dump-ratio (x file)
  (sub-dump-object (numerator x) file)
  (sub-dump-object (denominator x) file)
  (dump-fop 'lisp::fop-ratio file))

;;; Dump a long-float in the current *backend* format which may
;;; require conversion from the native backend format.
;;;
#+(and long-float x86)
(defun dump-long-float (float file)
  (declare (long-float float))
  (let ((exp-bits (long-float-exp-bits float))
	(high-bits (long-float-high-bits float))
	(low-bits (long-float-low-bits float)))
    (cond ((backend-featurep :x86)	; Native dump.
	   (dump-unsigned-32 low-bits file)
	   (dump-unsigned-32 high-bits file)
	   (dump-var-signed exp-bits 2 file))
	  ((backend-featurep :sparc)
	   ;; Some format converstion will be needed, just dump 0l0
	   ;; for now.
	   (unless (zerop float)
	     (format t "Warning: dumping ~s as 0l0~%" float))
	   (dump-unsigned-32 0 file)
	   (dump-unsigned-32 0 file)
	   (dump-unsigned-32 0 file)
	   (dump-var-signed 0 4 file))
	  (t
	   (error "Unable to dump long-float")))))

#+(and long-float sparc)
(defun dump-long-float (float file)
  (declare (long-float float))
  (let ((exp-bits (long-float-exp-bits float))
	(high-bits (long-float-high-bits float))
	(mid-bits (long-float-mid-bits float))
	(low-bits (long-float-low-bits float)))
    (cond ((backend-featurep :sparc)	; Native dump
	   (dump-unsigned-32 low-bits file)
	   (dump-unsigned-32 mid-bits file)
	   (dump-unsigned-32 high-bits file)
	   (dump-var-signed exp-bits 4 file))
	  (t
	   (error "Unable to dump long-float")))))

;;; Or a complex...

(defun dump-complex (x file)
  (typecase x
    ((complex single-float)
     (dump-fop 'lisp::fop-complex-single-float file)
     (dump-var-signed (single-float-bits (realpart x)) 4 file)
     (dump-var-signed (single-float-bits (imagpart x)) 4 file))
    ((complex double-float)
     (dump-fop 'lisp::fop-complex-double-float file)
     (let ((re (realpart x)))
       (declare (double-float re))
       (dump-unsigned-32 (double-float-low-bits re) file)
       (dump-var-signed (double-float-high-bits re) 4 file))
     (let ((im (imagpart x)))
       (declare (double-float im))
       (dump-unsigned-32 (double-float-low-bits im) file)
       (dump-var-signed (double-float-high-bits im) 4 file)))
    #+long-float
    ((complex long-float)
     (dump-fop 'lisp::fop-complex-long-float file)
     (dump-long-float (realpart x) file)
     (dump-long-float (imagpart x) file))
    (t
     (sub-dump-object (realpart x) file)
     (sub-dump-object (imagpart x) file)
     (dump-fop 'lisp::fop-complex file))))

;;; Dump an integer.

(defun dump-integer (n file)
  (typecase n
    ((signed-byte 8)
     (dump-fop 'lisp::fop-byte-integer file)
     (dump-byte (logand #xFF n) file))
    ((unsigned-byte 31)
     (dump-fop 'lisp::fop-word-integer file)
     (dump-unsigned-32 n file))
    ((signed-byte 32)
     (dump-fop 'lisp::fop-word-integer file)
     (dump-var-signed n 4 file))
    (t
     (let ((bytes (ceiling (1+ (integer-length n)) 8)))
       (dump-fop* bytes lisp::fop-small-integer lisp::fop-integer file)
       (dump-var-signed n bytes file)))))

(defun dump-float (x file)
  (etypecase x
    (single-float
     (dump-fop 'lisp::fop-single-float file)
     (dump-var-signed (single-float-bits x) 4 file))
    (double-float
     (dump-fop 'lisp::fop-double-float file)
     (let ((x x))
       (declare (double-float x))
       (dump-unsigned-32 (double-float-low-bits x) file)
       (dump-var-signed (double-float-high-bits x) 4 file)))
    #+long-float
    (long-float
     (dump-fop 'lisp::fop-long-float file)
     (dump-long-float x file))))


;;;; Symbol Dumping.

;;; Dump-Package  --  Internal
;;;
;;; Return the table index of Pkg, adding the package to the table if
;;; necessary.  During cold load, we read the string as a normal string so
;;; that we can do the package lookup at cold load time.
;;;
(defun dump-package (pkg file)
  (declare (type package pkg) (type fasl-file file) (values index)
	   (inline assoc))
  (cond ((cdr (assoc pkg (fasl-file-packages file) :test #'eq)))
	(t
	 (unless *cold-load-dump*
	   (dump-fop 'lisp::fop-normal-load file))
	 (dump-simple-string (package-name pkg) file)
	 (dump-fop 'lisp::fop-package file)
	 (unless *cold-load-dump*
	   (dump-fop 'lisp::fop-maybe-cold-load file))
	 (let ((entry (dump-pop file)))
	   (push (cons pkg entry) (fasl-file-packages file))
	   entry))))

;;; Dump-Symbol  --  Internal
;;;
;;; If we get here, it is assumed that the symbol isn't in the table, but
;;; we are responsible for putting it there when appropriate.  To avoid too
;;; much special-casing, we always push the symbol in the table, but don't
;;; record that we have done so if *Cold-Load-Dump* is true.
;;;
(defun dump-symbol (s file)
  (let* ((pname (symbol-name s))
	 (pname-length (length pname))
	 (pkg (symbol-package s)))

    (cond ((null pkg)
	   (dump-fop* pname-length lisp::fop-uninterned-small-symbol-save
		      lisp::fop-uninterned-symbol-save file))
	  ((eq pkg *package*)
	   (dump-fop* pname-length lisp::fop-small-symbol-save
		      lisp::fop-symbol-save file))
	  ((eq pkg ext:*lisp-package*)
	   (dump-fop* pname-length lisp::fop-lisp-small-symbol-save
		      lisp::fop-lisp-symbol-save file))
	  ((eq pkg ext:*keyword-package*)
	   (dump-fop* pname-length lisp::fop-keyword-small-symbol-save
		      lisp::fop-keyword-symbol-save file))
	  ((< pname-length 256)
	   (dump-fop* (dump-package pkg file)
		      lisp::fop-small-symbol-in-byte-package-save
		      lisp::fop-small-symbol-in-package-save file)
	   (dump-byte pname-length file))
	  (t
	   (dump-fop* (dump-package pkg file)
		      lisp::fop-symbol-in-byte-package-save
		      lisp::fop-symbol-in-package-save file)
	   (dump-unsigned-32 pname-length file)))

    (dump-bytes pname (length pname) file)

    (unless *cold-load-dump*
      (setf (gethash s (fasl-file-eq-table file)) (fasl-file-table-free file)))

    (incf (fasl-file-table-free file)))

  (undefined-value))


;;; Dumper for lists.

;;; Dump-List  --  Internal
;;;
;;; Dump a list, setting up patching information when there are
;;; circularities.  We scan down the list, checking for CDR and CAR
;;; circularities.
;;;
;;; If there is a CDR circularity, we terminate the list with NIL and make a
;;; Circularity notation for the CDR of the previous cons.
;;;
;;; If there is no CDR circularity, then we mark the current cons and check for
;;; a CAR circularity.  When there is a CAR circularity, we make the CAR NIL
;;; initially, arranging for the current cons to be patched later.
;;;
;;; Otherwise, we recursively call the dumper to dump the current element.
;;;
;;; Marking of the conses is inhibited when *cold-load-dump* is true.  This
;;; inhibits all circularity detection.
;;;
(defun dump-list (list file)
  (assert (and list
	       (not (gethash list (fasl-file-circularity-table file)))))
  (do* ((l list (cdr l))
	(n 0 (1+ n))
	(circ (fasl-file-circularity-table file)))
       ((atom l)
	(cond ((null l)
	       (terminate-undotted-list n file))
	      (t
	       (sub-dump-object l file)
	       (terminate-dotted-list n file))))
    (declare (type index n))
    (let ((ref (gethash l circ)))
      (when ref
	(push (make-circularity :type :rplacd  :object list  :index (1- n)
				:value l  :enclosing-object ref)
	      *circularities-detected*)
	(terminate-undotted-list n file)
	(return)))

    (unless *cold-load-dump*
      (setf (gethash l circ) list))

    (let* ((obj (car l))
	   (ref (gethash obj circ)))
      (cond (ref
	     (push (make-circularity :type :rplaca  :object list  :index n
				     :value obj  :enclosing-object ref)
		   *circularities-detected*)
	     (sub-dump-object nil file))
	    (t
	     (sub-dump-object obj file))))))

(defun terminate-dotted-list (n file)
  (declare (type index n) (type fasl-file file))
  (case n
    (1 (dump-fop 'lisp::fop-list*-1 file))
    (2 (dump-fop 'lisp::fop-list*-2 file))
    (3 (dump-fop 'lisp::fop-list*-3 file))
    (4 (dump-fop 'lisp::fop-list*-4 file))
    (5 (dump-fop 'lisp::fop-list*-5 file))
    (6 (dump-fop 'lisp::fop-list*-6 file))
    (7 (dump-fop 'lisp::fop-list*-7 file))
    (8 (dump-fop 'lisp::fop-list*-8 file))
    (T (do ((nn n (- nn 255)))
	   ((< nn 256)
	    (dump-fop 'lisp::fop-list* file)
	    (dump-byte nn file))
	 (declare (type index nn))
	 (dump-fop 'lisp::fop-list* file)
	 (dump-byte 255 file)))))

;;; If N > 255, must build list with one list operator, then list* operators.

(defun terminate-undotted-list (n file)
  (declare (type index n) (type fasl-file file))
  (case n
    (1 (dump-fop 'lisp::fop-list-1 file))
    (2 (dump-fop 'lisp::fop-list-2 file))
    (3 (dump-fop 'lisp::fop-list-3 file))
    (4 (dump-fop 'lisp::fop-list-4 file))
    (5 (dump-fop 'lisp::fop-list-5 file))
    (6 (dump-fop 'lisp::fop-list-6 file))
    (7 (dump-fop 'lisp::fop-list-7 file))
    (8 (dump-fop 'lisp::fop-list-8 file))
    (T (cond ((< n 256)
	      (dump-fop 'lisp::fop-list file)
	      (dump-byte n file))
	     (t (dump-fop 'lisp::fop-list file)
		(dump-byte 255 file)
		(do ((nn (- n 255) (- nn 255)))
		    ((< nn 256)
		     (dump-fop 'lisp::fop-list* file)
		     (dump-byte nn file))
		  (declare (type index nn))
		  (dump-fop 'lisp::fop-list* file)
		  (dump-byte 255 file)))))))


;;;; Array dumping.

;;; DUMP-ARRAY  --  Internal.
;;;
;;; Dump the array thing.
;;;
(defun dump-array (x file)
  (if (vectorp x)
      (dump-vector x file)
      (dump-multi-dim-array x file)))

;;; DUMP-VECTOR  --  Internal.
;;;
;;; Dump the vector object.  If it's not simple, then actually dump a simple
;;; version of it.  But we enter the original in the EQ or EQUAL tables.
;;;
(defun dump-vector (x file)
  (let ((simple-version (if (array-header-p x)
			    (coerce x 'simple-array)
			    x)))
    (typecase simple-version
      (simple-base-string
       (if *coalesce-constants*
	   (unless (equal-check-table x file)
	     (dump-simple-string simple-version file)
	     (equal-save-object x file))
	   (dump-simple-string simple-version file)))
      (simple-vector
       (dump-simple-vector simple-version file)
       (eq-save-object x file))
      ((simple-array single-float (*))
       (dump-single-float-vector simple-version file)
       (eq-save-object x file))
      ((simple-array double-float (*))
       (dump-double-float-vector simple-version file)
       (eq-save-object x file))
      #+long-float
      ((simple-array long-float (*))
       (dump-long-float-vector simple-version file)
       (eq-save-object x file))
      ((simple-array (complex single-float) (*))
       (dump-complex-single-float-vector simple-version file)
       (eq-save-object x file))
      ((simple-array (complex double-float) (*))
       (dump-complex-double-float-vector simple-version file)
       (eq-save-object x file))
      #+long-float
      ((simple-array (complex long-float) (*))
       (dump-complex-long-float-vector simple-version file)
       (eq-save-object x file))
      (t
       (dump-i-vector simple-version file)
       (eq-save-object x file)))))

;;; DUMP-SIMPLE-VECTOR  --  Internal
;;;
;;; Dump a SIMPLE-VECTOR, handling any circularities.
;;;
(defun dump-simple-vector (v file)
  (declare (type simple-vector v) (type fasl-file file))
  (note-potential-circularity v file)
  (do ((index 0 (1+ index))
       (length (length v))
       (circ (fasl-file-circularity-table file)))
      ((= index length)
       (dump-fop* length lisp::fop-small-vector lisp::fop-vector file))
    (let* ((obj (aref v index))
	   (ref (gethash obj circ)))
      (cond (ref
	     (push (make-circularity :type :svset  :object v  :index index
				     :value obj  :enclosing-object ref)
		   *circularities-detected*)
	     (sub-dump-object nil file))
	    (t
	     (sub-dump-object obj file))))))

;;; DUMP-SIMPLE-STRING  --  Internal
;;;
;;; Dump a SIMPLE-BASE-STRING.
;;;
(defun dump-simple-string (s file)
  (declare (type simple-base-string s))
  (let ((length (length s)))
    (dump-fop* length lisp::fop-small-string lisp::fop-string file)
    (dump-bytes s length file))
  (undefined-value))

;;; DUMP-I-VECTOR  --  Internal
;;;
;;; *** NOT *** the FOP-INT-VECTOR as currently documented in rtguts.  Size
;;; must be a directly supported I-vector element size, with no extra bits.
;;;
;;; If a byte vector, or if the native and target byte orderings are the same,
;;; then just write the bits.  Otherwise, dispatch off of the target byte order
;;; and write the vector one element at a time.
;;;
(defun dump-i-vector (vec file &optional data-only)
  (declare (type (simple-array * (*)) vec))
  (let ((len (length vec)))
    (labels ((dump-unsigned (size bytes)
	       (unless data-only
		 (dump-fop 'lisp::fop-int-vector file)
		 (dump-unsigned-32 len file)
		 (dump-byte size file))
	       (dump-data-maybe-byte-swapping vec bytes size file))
	     (dump-signed (size dump-size bytes)
	       (unless data-only
		 (dump-fop 'lisp::fop-signed-int-vector file)
		 (dump-unsigned-32 len file)
		 (dump-byte size file))
	       (dump-data-maybe-byte-swapping vec bytes dump-size file)))
      (etypecase vec
	(simple-bit-vector
	 (dump-unsigned 1 (ash (+ (the index len) 7) -3)))
	((simple-array (unsigned-byte 2) (*))
	 (dump-unsigned 2 (ash (+ (the index (ash len 1)) 7) -3)))
	((simple-array (unsigned-byte 4) (*))
	 (dump-unsigned 4 (ash (+ (the index (ash len 2)) 7) -3)))
	((simple-array (unsigned-byte 8) (*))
	 (dump-unsigned 8 len))
	((simple-array (unsigned-byte 16) (*))
	 (dump-unsigned 16 (* 2 len)))
	((simple-array (unsigned-byte 32) (*))
	 (dump-unsigned 32 (* 4 len)))
	((simple-array (signed-byte 8) (*))
	 (dump-signed 8 8 len))
	((simple-array (signed-byte 16) (*))
	 (dump-signed 16 16 (* 2 len)))
	((simple-array (signed-byte 30) (*))
	 (dump-signed 30 32 (* 4 len)))
	((simple-array (signed-byte 32) (*))
	 (dump-signed 32 32 (* 4 len)))))))

;;; DUMP-SINGLE-FLOAT-VECTOR  --  internal.
;;;
(defun dump-single-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'lisp::fop-single-float-vector file)
    (dump-unsigned-32 length file)
    (dump-data-maybe-byte-swapping vec (* length vm:word-bytes)
				   vm:word-bytes file)))

;;; DUMP-DOUBLE-FLOAT-VECTOR  --  internal.
;;;
(defun dump-double-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'lisp::fop-double-float-vector file)
    (dump-unsigned-32 length file)
    (dump-data-maybe-byte-swapping vec (* length vm:word-bytes 2)
				   (* vm:word-bytes 2) file)))

;;; DUMP-LONG-FLOAT-VECTOR  --  internal.
;;;
#+long-float
(defun dump-long-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'lisp::fop-long-float-vector file)
    (dump-unsigned-32 length file)
    (dump-data-maybe-byte-swapping
     vec (* length vm:word-bytes #+x86 3 #+sparc 4)
     (* vm:word-bytes #+x86 3 #+sparc 4) file)))

;;; DUMP-COMPLEX-SINGLE-FLOAT-VECTOR  --  internal.
;;;
(defun dump-complex-single-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'lisp::fop-complex-single-float-vector file)
    (dump-unsigned-32 length file)
    (dump-data-maybe-byte-swapping vec (* length vm:word-bytes 2)
				   vm:word-bytes file)))

;;; DUMP-COMPLEX-DOUBLE-FLOAT-VECTOR  --  internal.
;;;
(defun dump-complex-double-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'lisp::fop-complex-double-float-vector file)
    (dump-unsigned-32 length file)
    (dump-data-maybe-byte-swapping vec (* length vm:word-bytes 2 2)
				   (* vm:word-bytes 2) file)))

;;; DUMP-COMPLEX-LONG-FLOAT-VECTOR  --  internal.
;;;
#+long-float
(defun dump-complex-long-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'lisp::fop-complex-long-float-vector file)
    (dump-unsigned-32 length file)
    (dump-data-maybe-byte-swapping
     vec (* length vm:word-bytes #+x86 3 #+sparc 4 2)
     (* vm:word-bytes #+x86 3 #+sparc 4) file)))

;;; DUMP-DATA-BITS-MAYBE-BYTE-SWAPPING  --  internal.
;;;
;;; Dump BYTES of data from DATA-VECTOR (which must be some unboxed vector)
;;; byte-swapping if necessary.
;;;
(defun dump-data-maybe-byte-swapping (data-vector bytes element-size file)
  (declare (type (simple-array * (*)) data-vector)
	   (type unsigned-byte bytes)
	   (type (integer 1) element-size))
  (cond ((or (eq (backend-byte-order *backend*)
		 (backend-byte-order *native-backend*))
	     (= element-size vm:byte-bits))
	 (dump-bytes data-vector bytes file))
	((>= element-size vm:word-bits)
	 (let* ((words-per-element (/ element-size vm:word-bits))
		(bytes-per-element (* words-per-element vm:word-bytes))
		(elements (/ bytes bytes-per-element))
		(result (make-array bytes :element-type '(unsigned-byte 8))))
	   (declare (type (integer 1 #.most-positive-fixnum)
			  words-per-element bytes-per-element elements))
	   (dotimes (index elements)
	     (dotimes (offset words-per-element)
	       (let ((word (%raw-bits data-vector
				      (+ (* index words-per-element)
					 vm:vector-data-offset
					 (1- words-per-element)
					 (- offset)))))
		 (setf (%raw-bits result (+ (* index words-per-element)
					    vm:vector-data-offset
					    offset))
		       (logior (ash (ldb (byte 8 0) word) 24)
			       (ash (ldb (byte 8 8) word) 16)
			       (ash (ldb (byte 8 16) word) 8)
			       (ldb (byte 8 24) word))))))
	   (dump-bytes result bytes file)))
	((> element-size vm:byte-bits)
	 (let* ((bytes-per-element (/ element-size vm:byte-bits))
		(elements (/ bytes bytes-per-element))
		(result (make-array elements
				    :element-type
				    `(unsigned-byte ,element-size))))
	   (declare (type (integer 1 #.most-positive-fixnum)
			  bytes-per-element)
		    (type unsigned-byte elements))
	   (dotimes (index elements)
	     (let ((element (aref data-vector index))
		   (new-element 0))
	       (dotimes (i bytes-per-element)
		 (setf new-element
		       (logior (ash new-element vm:byte-bits)
			       (ldb (byte vm:byte-bits 0) element)))
		 (setf element (ash element (- vm:byte-bits))))
	       (setf (aref result index) new-element)))
	   (dump-bytes result bytes file)))
	(t
	 (let* ((elements-per-byte (/ vm:byte-bits element-size))
		(elements (* bytes elements-per-byte))
		(len (length data-vector))
		(result (make-array elements
				    :element-type
				    `(unsigned-byte ,element-size))))
	   (dotimes (index elements)
	     (multiple-value-bind (byte-index additional)
				  (truncate index elements-per-byte)
	       (let ((src-idx (- (* (1+ byte-index) elements-per-byte)
				 (1+ additional))))
		 (setf (aref result index)
		       (if (>= src-idx len)
			   0
			   (aref data-vector src-idx))))))
	   (dump-bytes result bytes file)))))

;;; Dump-Multi-Dim-Array  --  Internal
;;;
;;; Dump a multi-dimensional array.  Note: any displacements are folded out.
;;;
(defun dump-multi-dim-array (array file)
  (let ((rank (array-rank array)))
    (dotimes (i rank)
      (dump-integer (array-dimension array i) file))
    (lisp::with-array-data ((vector array) (start) (end))
      (if (and (= start 0) (= end (length vector)))
	  (sub-dump-object vector file)
	  (sub-dump-object (subseq vector start end) file)))
    (dump-fop 'lisp::fop-array file)
    (dump-unsigned-32 rank file)
    (eq-save-object array file)))


;;; Dump a character.

(defun dump-character (ch file)
  (dump-fop 'lisp::fop-short-character file)
  (dump-byte (char-code ch) file))


;;; Dump a structure.

(defun dump-structure (struct file)
  (when *dump-only-valid-structures*
    (unless (gethash struct (fasl-file-valid-structures file))
      (error "Attempt to dump invalid structure:~%  ~S~%How did this happen?"
	     struct)))
  (note-potential-circularity struct file)
  (do ((index 0 (1+ index))
       (length (%instance-length struct))
       (circ (fasl-file-circularity-table file)))
      ((= index length)
       (dump-fop* length lisp::fop-small-struct lisp::fop-struct file))
    (let* ((obj #+ns-boot
		(if (zerop index)
		    (%instance-layout struct)
		    (%instance-ref struct index))
		#-ns-boot
		(%instance-ref struct index))
	   (ref (gethash obj circ)))
      (cond (ref
	     (push (make-circularity :type :struct-set
				     :object struct
				     :index index
				     :value obj
				     :enclosing-object ref)
		   *circularities-detected*)
	     (sub-dump-object nil file))
	    (t
	     (sub-dump-object obj file))))))

(defun dump-layout (obj file)
  (unless (member (layout-invalid obj) '(nil :compiler))
    (compiler-error "Dumping reference to obsolete class: ~S"
		    (layout-class obj)))
  (let ((name (class-name (layout-class obj))))
    (assert name)
    (dump-fop 'lisp::fop-normal-load file)
    (let ((*cold-load-dump* t))
      (dump-object name file))
    (dump-fop 'lisp::fop-maybe-cold-load file))
  (sub-dump-object (layout-inherits obj) file)
  (sub-dump-object (layout-inheritance-depth obj) file)
  (sub-dump-object (layout-length obj) file)
  (dump-fop 'lisp::fop-layout file))


#[ Fasload File Format

== General ==

The purpose of Fasload files is to allow concise storage and rapid loading
of Lisp data, particularly function definitions.  The intent is that
loading a Fasload file has the same effect as loading the ASCII file from
which the Fasload file was compiled, but accomplishes the tasks more
efficiently.  One noticeable difference, of course, is that function
definitions may be in compiled form rather than S-expression form.  Another
is that Fasload files may specify in what parts of memory the Lisp data
should be allocated.  For example, constant lists used by compiled code may
be regarded as read-only.

In some Lisp implementations, Fasload file formats are designed to allow
sharing of code parts of the file, possibly by direct mapping of pages of
the file into the address space of a process.  This technique produces
great performance improvements in a paged time-sharing system.  Since the
Mach project is to produce a distributed personal-computer network system
rather than a time-sharing system, efficiencies of this type are explicitly
{\it not} a goal for the CMU Common Lisp Fasload file format.

On the other hand, CMU Common Lisp is intended to be portable, as it will
eventually run on a variety of machines.  Therefore an explicit goal is
that Fasload files shall be transportable among various implementations, to
permit efficient distribution of programs in compiled form.  The
representations of data objects in Fasload files shall be relatively
independent of such considerations as word length, number of type bits, and
so on.  If two implementations interpret the same macrocode (compiled code
format), then Fasload files should be completely compatible.  If they do
not, then files not containing compiled code (so-called "Fasdump" data
files) should still be compatible.  While this may lead to a format which
is not maximally efficient for a particular implementation, the sacrifice
of a small amount of performance is deemed a worthwhile price to pay to
achieve portability.

The primary assumption about data format compatibility is that all
implementations can support I/O on finite streams of eight-bit bytes.  By
"finite" we mean that a definite end-of-file point can be detected
irrespective of the content of the data stream.  A Fasload file will be
regarded as such a byte stream.

== Strategy ==

A Fasload file may be regarded as a human-readable prefix followed by code
in a funny little language.  When interpreted, this code will cause the
construction of the encoded data structures.  The virtual machine which
interprets this code has a {\it stack} and a {\it table}, both initially
empty.  The table may be thought of as an expandable register file; it is
used to remember quantities which are needed more than once.  The elements
of both the stack and the table are Lisp data objects.  Operators of the
funny language may take as operands following bytes of the data stream, or
items popped from the stack.  Results may be pushed back onto the stack or
pushed onto the table.  The table is an indexable stack that is never
popped; it is indexed relative to the base, not the top, so that an item
once pushed always has the same index.

More precisely, a Fasload file has the following macroscopic organization.
It is a sequence of zero or more groups concatenated together.  End-of-file
must occur at the end of the last group.  Each group begins with a series
of seven-bit ASCII characters terminated by one or more bytes of all ones
\verb|#xFF|; this is called the {\it header}.  Following the bytes which
terminate the header is the {\it body}, a stream of bytes in the funny
binary language.  The body of necessity begins with a byte other than
\verb|#xFF|.  The body is terminated by the operation {\tt FOP-END-GROUP}.

The first nine characters of the header must be "{\tt FASL FILE}" in
upper-case letters.  The rest may be any ASCII text, but by convention it
is formatted in a certain way.  The header is divided into lines, which are
grouped into paragraphs.  A paragraph begins with a line which does {\it
not} begin with a space or tab character, and contains all lines up to, but
not including, the next such line.  The first word of a paragraph, defined
to be all characters up to but not including the first space, tab, or
end-of-line character, is the {\it name} of the paragraph.  A Fasload file
header might look something like this:

    FASL FILE >SteelesPerq>User>Guy>IoHacks>Pretty-Print.Slisp
    Package Pretty-Print
    Compiled 31-Mar-1988 09:01:32 by some random luser
    Compiler Version 1.6, Lisp Version 3.0.
    Functions: INITIALIZE DRIVER HACK HACK1 MUNGE MUNGE1 GAZORCH
               MINGLE MUDDLE PERTURB OVERDRIVE GOBBLE-KEYBOARD
               FRY-USER DROP-DEAD HELP CLEAR-MICROCODE
                %AOS-TRIANGLE %HARASS-READTABLE-MAYBE
    Macros:    PUSH POP FROB TWIDDLE

{\it one or more bytes of \verb|#xFF|}

The particular paragraph names and contents shown here are only intended as
suggestions.

== Fasload Language ==

Each operation in the binary Fasload language is an eight-bit (one-byte)
opcode.  Each has a name beginning with "{\tt FOP-}".  In the following
descriptions, the name is followed by operand descriptors.  Each descriptor
denotes operands that follow the opcode in the input stream.  A quantity in
parentheses indicates the number of bytes of data from the stream making up
the operand.  Operands which implicitly come from the stack are noted in
the text.  The notation "$\Rightarrow$ stack" means that the result is
pushed onto the stack; "$\Rightarrow$ table" similarly means that the
result is added to the table.  A construction like "{\it n}(1) {\it
value}({\it n})" means that first a single byte {\it n} is read from the
input stream, and this byte specifies how many bytes to read as the operand
named {\it value}.  All numeric values are unsigned binary integers unless
otherwise specified.  Values described as "signed" are in two's-complement
form unless otherwise specified.  When an integer read from the stream
occupies more than one byte, the first byte read is the least significant
byte, and the last byte read is the most significant (and contains the sign
bit as its high-order bit if the entire integer is signed).

Some of the operations are not necessary, but are rather special cases of
or combinations of others.  These are included to reduce the size of the
file or to speed up important cases.  As an example, nearly all strings are
less than 256 bytes long, and so a special form of string operation might
take a one-byte length rather than a four-byte length.  As another example,
some implementations may choose to store bits in an array in a
left-to-right format within each word, rather than right-to-left.  The
Fasload file format may support both formats, with one being significantly
more efficient than the other for a given implementation.  The compiler for
any implementation may generate the more efficient form for that
implementation, and yet compatibility can be maintained by requiring all
implementations to support both formats in Fasload files.

Measurements are to be made to determine which operation codes are
worthwhile; little-used operations may be discarded and new ones added.
After a point the definition will be "frozen", meaning that existing
operations may not be deleted (though new ones may be added; some
operations codes will be reserved for that purpose).

  % 0: FOP-NOP

    No operation.  (This is included because it is recognized
    that some implementations may benefit from alignment of operands to some
    operations, for example to 32-bit boundaries.  This operation can be used
    to pad the instruction stream to a desired boundary.)

  % 1: FOP-POP -> table

    One item is popped from the stack and added to the table.

  % 2: FOP-PUSH index(4) -> stack

    Item number index of the table is pushed onto the stack.
    The first element of the table is item number zero.

  % 3: FOP-BYTE-PUSH index(1) -> stack

    Item number index of the table is pushed onto the stack.
    The first element of the table is item number zero.

  % 4: FOP-EMPTY-LIST -> stack

    The empty list (()) is pushed onto the stack.

  % 5: FOP-TRUTH -> stack

    The standard truth value (T) is pushed onto the stack.

  % 6: FOP-SYMBOL-SAVE n(4) name(n) -> stack and table

    The four-byte operand n specifies the length of the print name of a
    symbol.  The name follows, one character per byte, with the first byte
    of the print name being the first read.  The name is interned in the
    default package, and the resulting symbol is both pushed onto the stack
    and added to the table.

  % 7: FOP-SMALL-SYMBOL-SAVE n(1) name(n) -> stack and table

    The one-byte operand n specifies the length of the print name of a
    symbol.  The name follows, one character per byte, with the first byte
    of the print name being the first read.  The name is interned in the
    default package, and the resulting symbol is both pushed onto the stack
    and added to the table.

  % 8: FOP-SYMBOL-IN-PACKAGE-SAVE index(4) n(4) name(n) -> stack and table

    The four-byte index specifies a package stored in the table.  The
    four-byte operand n specifies the length of the print name of a symbol.
    The name follows, one character per byte, with the first byte of the
    print name being the first read.  The name is interned in the specified
    package, and the resulting symbol is both pushed onto the stack and
    added to the table.

  % 9: FOP-SMALL-SYMBOL-IN-PACKAGE-SAVE  index(4) n(1) name(n) -> stack and table

    The four-byte index specifies a package stored in the table.  The
    one-byte operand n specifies the length of the print name of a symbol.
    The name follows, one character per byte, with the first byte of the
    print name being the first read.  The name is interned in the specified
    package, and the resulting symbol is both pushed onto the stack and
    added to the table.

  % 10: FOP-SYMBOL-IN-BYTE-PACKAGE-SAVE index(1) n(4) name(n) -> stack and table

    The one-byte index specifies a package stored in the table.  The
    four-byte operand n specifies the length of the print name of a symbol.
    The name follows, one character per byte, with the first byte of the
    print name being the first read.  The name is interned in the specified
    package, and the resulting symbol is both pushed onto the stack and
    added to the table.

  % 11: FOP-SMALL-SYMBOL-IN-BYTE-PACKAGE-SAVE index(1) n(1) name(n) -> stack and table

    The one-byte index specifies a package stored in the table.  The
    one-byte operand n specifies the length of the print name of a symbol.
    The name follows, one character per byte, with the first byte of the
    print name being the first read.  The name is interned in the specified
    package, and the resulting symbol is both pushed onto the stack and
    added to the table.

  % 12: FOP-UNINTERNED-SYMBOL-SAVE n(4) name(n) -> stack and table

    Like FOP-SYMBOL-SAVE, except that it creates an uninterned symbol.

  % 13: FOP-UNINTERNED-SMALL-SYMBOL-SAVE n(1) name(n) -> stack and table

    Like FOP-SMALL-SYMBOL-SAVE, except that it creates an uninterned symbol.

  % 14: FOP-PACKAGE -> table

    An item is popped from the stack; it must be a symbol.  The package of
    that name is located and pushed onto the table.

  % 15: FOP-LIST length(1) -> stack

    The unsigned operand length specifies a number of operands to be popped
    from the stack.  These are made into a list of that length, and the
    list is pushed onto the stack.  The first item popped from the stack
    becomes the last element of the list, and so on.  Hence an iterative
    loop can start with the empty list and perform "pop an item and cons it
    onto the list" length times.  (Lists of length greater than 255 can be
    made by using FOP-LIST* repeatedly.)

  % 16: FOP-LIST* length(1) -> stack

    This is like FOP-LIST except that the constructed list is terminated
    not by () (the empty list), but by an item popped from the stack before
    any others are.  Therefore length+1 items are popped in all.  Hence an
    iterative loop can start with a popped item and perform "pop an item
    and cons it onto the list" length+1 times.

  % 17-24: FOP-LIST-1, FOP-LIST-2, ..., FOP-LIST-8

    FOP-LIST-k is like FOP-LIST with a byte containing k following it.
    These exist purely to reduce the size of Fasload files.  Measurements
    need to be made to determine the useful values of k.

  % 25-32: FOP-LIST*-1, FOP-LIST*-2, ..., FOP-LIST*-8

    FOP-LIST*-k is like FOP-LIST* with a byte containing k
    following it.  These exist purely to reduce the size of Fasload files.
    Measurements need to be made to determine the useful values of k.

  % 33: FOP-INTEGER n(4) value(n) -> stack

    A four-byte unsigned operand specifies the number of following bytes.
    These bytes define the value of a signed integer in two's-complement
    form.  The first byte of the value is the least significant byte.

  % 34: FOP-SMALL-INTEGER n(1) value(n) -> stack

    A one-byte unsigned operand specifies the number of following
    bytes.  These bytes define the value of a signed integer in two's-complement
    form.  The first byte of the value is the least significant byte.

  % 35: FOP-WORD-INTEGER value(4) -> stack

    A four-byte signed integer (in the range $-2^{31$ to $2^{31-1$) follows the
    operation code.  A LISP integer (fixnum or bignum) with that value
    is constructed and pushed onto the stack.

  % 36: FOP-BYTE-INTEGER value(1) -> stack

    A one-byte signed integer (in the range -128 to 127) follows the
    operation code.  A LISP integer (fixnum or bignum) with that value
    is constructed and pushed onto the stack.

  % 37: FOP-STRING n(4) name(n) -> stack

    The four-byte operand n specifies the length of a string to
    construct.  The characters of the string follow, one per byte.
    The constructed string is pushed onto the stack.

  % 38: FOP-SMALL-STRING n(1) name(n) -> stack

    The one-byte operand n specifies the length of a string to
    construct.  The characters of the string follow, one per byte.
    The constructed string is pushed onto the stack.

  % 39: FOP-VECTOR n(4) -> stack

    The four-byte operand n specifies the length of a vector of LISP objects
    to construct.  The elements of the vector are popped off the stack;
    the first one popped becomes the last element of the vector.
    The constructed vector is pushed onto the stack.

  % 40: FOP-SMALL-VECTOR n(1) -> stack

    The one-byte operand n specifies the length of a vector of LISP objects
    to construct.  The elements of the vector are popped off the stack;
    the first one popped becomes the last element of the vector.
    The constructed vector is pushed onto the stack.

  % 41: FOP-UNIFORM-VECTOR n(4) -> stack

    The four-byte operand n specifies the length of a vector of LISP objects
    to construct.  A single item is popped from the stack and used to initialize
    all elements of the vector.  The constructed vector is pushed onto the stack.

  % 42: FOP-SMALL-UNIFORM-VECTOR n(1) -> stack

    The one-byte operand n specifies the length of a vector of LISP objects
    to construct.  A single item is popped from the stack and used to initialize
    all elements of the vector.  The constructed vector is pushed onto the stack.

  % 43: FOP-INT-VECTOR len(4) size(1) data($\left\lceil len*count/8\right\rceil$) -> stack

    The four-byte operand n specifies the length of a vector of unsigned
    integers to be constructed.  Each integer is size bits long, and is
    packed according to the machine's native byte ordering.  size must be a
    directly supported i-vector element size.  Currently supported values
    are 1,2,4,8,16 and 32.

  % 44: FOP-UNIFORM-INT-VECTOR n(4) size(1) value(@ceiling<size/8>) -> stack

    The four-byte operand n specifies the length of a vector of unsigned
    integers to construct.  Each integer is size bits big, and is
    initialized to the value of the operand value.  The constructed vector
    is pushed onto the stack.

  % 45: Unused

  % 46: FOP-SINGLE-FLOAT data(4) -> stack

    The data bytes are read as an integer, then turned into an IEEE single
    float (as though by make-single-float).

  % 47: FOP-DOUBLE-FLOAT data(8) -> stack

    The data bytes are read as an integer, then turned into an IEEE double
    float (as though by make-double-float).

  % 48: FOP-STRUCT n(4) -> stack

    The four-byte operand n specifies the length FIX? structure to construct.  The
    elements of the vector are popped off the stack; the first one popped becomes
    the last element of the structure.  The constructed vector is pushed onto the
    stack.

  % 49: FOP-SMALL-STRUCT n(1) -> stack

    The one-byte operand n specifies the length structure to construct.  The
    elements of the vector are popped off the stack; the first one popped becomes
    the last element of the structure.  The constructed vector is pushed onto the
    stack.

  % 50-52: Unused

  % 53: FOP-EVAL -> stack

    Pop an item from the stack and evaluate it (give it to EVAL).
    Push the result back onto the stack.

  % 54: FOP-EVAL-FOR-EFFECT

    Pop an item from the stack and evaluate it (give it to EVAL).  The
    result is ignored.

  % 55: FOP-FUNCALL nargs(1) -> stack

    Pop nargs+1 items from the stack and apply the last one popped as a
    function to all the rest as arguments (the first one popped being the
    last argument).  Push the result back onto the stack.

  % 56: FOP-FUNCALL-FOR-EFFECT nargs(1)

    Pop nargs+1 items from the stack and apply the last one popped as a
    function to all the rest as arguments (the first one popped being the
    last argument).  The result is ignored.

  % 57: FOP-CODE-FORMAT implementation(1) version(1)

    This FOP specifiers the code format for following code objects.  The
    operations FOP-CODE and its relatives may not occur in a group until
    after FOP-CODE-FORMAT has appeared; there is no default format.  The
    implementation is an integer indicating the target hardware and
    environment.  See compiler/generic/vm-macs.lisp for the currently
    defined implementations.  version for an implementation is increased
    whenever there is a change that renders old fasl files unusable.

  % 58: FOP-CODE nitems(4) size(4) code(size) -> stack

    A compiled function is constructed and pushed onto the stack.  This
    object is in the format specified by the most recent occurrence of
    FOP-CODE-FORMAT.  The operand nitems specifies a number of items to pop
    off the stack to use in the "boxed storage" section.  The operand code
    is a string of bytes constituting the compiled executable code.

  % 59: FOP-SMALL-CODE nitems(1) size(2) code(size) -> stack

    A compiled function is constructed and pushed onto the stack.  This
    object is in the format specified by the most recent occurrence of
    FOP-CODE-FORMAT.  The operand nitems specifies a number of items to pop
    off the stack to use in the "boxed storage" section.  The operand code
    is a string of bytes constituting the compiled executable code.

  % 60-61: Unused

  % 62: FOP-VERIFY-TABLE-SIZE size(4)

    If the current size of the table is not equal to size, then an
    inconsistency has been detected.  This operation is inserted into a
    Fasload file purely for error-checking purposes.  It is good practice
    for a compiler to output this at least at the end of every group, if
    not more often.

  % 63: FOP-VERIFY-EMPTY-STACK

    If the stack is not currently empty, then an inconsistency has been
    detected.  This operation is inserted into a Fasload file purely for
    error-checking purposes.  It is good practice for a compiler to output
    this at least at the end of every group, if not more often.

  % 64: FOP-END-GROUP

    This is the last operation of a group.  If this is not the last byte of
    the file, then a new group follows; the next nine bytes must be "FASL
    FILE".

  % 65: FOP-POP-FOR-EFFECT stack ->

    One item is popped from the stack.

  % 66: FOP-MISC-TRAP -> stack

    A trap object is pushed onto the stack.

  % 67: Unused

  % 68: FOP-CHARACTER character(3) -> stack

    The three bytes are read as an integer then converted to a character.  This FOP
    is currently rather useless, as extended characters are not supported.

  % 69: FOP-SHORT-CHARACTER character(1) -> stack

    The one byte specifies the code of a Common Lisp character object.  A character
    is constructed and pushed onto the stack.

  % 70: FOP-RATIO -> stack

    Creates a ratio from two integers popped from the stack.
    The denominator is popped first, the numerator second.

  % 71: FOP-COMPLEX -> stack

    Creates a complex number from two numbers popped from the stack.
    The imaginary part is popped first, the real part second.

  % 72-73: Unused

  % 74: FOP-FSET

    Except in the cold loader (Genesis), this is a no-op with two stack arguments.
    In the initial core this is used to make DEFUN functions defined at cold-load
    time so that global functions can be called before top-level forms are run
    (which normally installs definitions.)  Genesis pops the top two things off
    the stack and effectively does (SETF SYMBOL-FUNCTION).

  % 75: FOP-LISP-SYMBOL-SAVE n(4) name(n) -> stack and table

    Like FOP-SYMBOL-SAVE, except that it creates a symbol in the LISP
    package.

  % 76: FOP-LISP-SMALL-SYMBOL-SAVE n(1) name(n) -> stack and table

    Like FOP-SMALL-SYMBOL-SAVE, except that it creates a symbol in the LISP
    package.

  % 77: FOP-KEYWORD-SYMBOL-SAVE n(4) name(n) -> stack and table

    Like FOP-SYMBOL-SAVE, except that it creates a symbol in the
    KEYWORD package.

  % 78: FOP-KEYWORD-SMALL-SYMBOL-SAVE n(1)  name(n) -> stack and table

    Like FOP-SMALL-SYMBOL-SAVE, except that it creates a symbol in the
    KEYWORD package.

  % 79-80: Unused

  % 81: FOP-NORMAL-LOAD

    This FOP is used in conjunction with the cold loader (Genesis) to read
    top-level package manipulation forms.  These forms are to be read as though by
    the normal loaded, so that they can be evaluated at cold load time, instead of
    being dumped into the initial core image.  A no-op in normal loading.

  % 82: FOP-MAYBE-COLD-LOAD

    Undoes the effect of FOP-NORMAL-LOAD.

  % 83: FOP-ARRAY rank(4) -> stack

    This operation creates a simple array header (used for simple-arrays with rank
    /= 1).  The data vector is popped off of the stack, and then rank
    dimensions are popped off of the stack (the highest dimensions is on top.)

  % 84-139: Unused

  % 140: FOP-ALTER-CODE index(4)

    This operation modifies the constants part of a code object (necessary for
    creating certain circular function references.)  It pops the new value and code
    object are off the stack, storing the new value at the specified index.

  % 141: FOP-BYTE-ALTER-CODE index(1)

    Like FOP-ALTER-CODE, but has only a one byte offset.

  % 142: FOP-FUNCTION-ENTRY index(4) -> stack

    Initializes a function-entry header inside of a pre-existing code object, and
    returns the corresponding function descriptor.  index is the byte offset
    inside of the code object where the header should be plunked down.  The stack
    arguments to this operation are the code object, function name, function debug
    arglist and function type.

  % 143: Unused

  % 144: FOP-ASSEMBLER-CODE length(4) -> stack

    This operation creates a code object holding assembly routines.  length
    bytes of code are read and placed in the code object, and the code object
    descriptor is pushed on the stack.  This FOP is only recognized by the cold
    loader (Genesis.)

  % 145: FOP-ASSEMBLER-ROUTINE offset(4) -> stack

    This operation records an entry point into an assembler code object
    (for use with FOP-ASSEMBLER-FIXUP).  The routine name (a symbol) is on
    stack top.  The code object is underneath.  The entry point is defined
    at offset bytes inside the code area of the code object, and the code
    object is left on stack top (allowing multiple uses of this FOP to be
    chained.)  This FOP is only recognized by the cold loader (Genesis.)

  % 146: Unused

  % 147: FOP-FOREIGN-FIXUP len(1) name(len) offset(4) -> stack

    This operation resolves a reference to a foreign (C) symbol.  len bytes
    are read and interpreted as the symbol name.  First the kind and the
    code-object to patch are popped from the stack.  The kind is a target-dependent
    symbol indicating the instruction format of the patch target (at offset
    bytes from the start of the code area.)  The code object is left on
    stack top (allowing multiple uses of this FOP to be chained.)

  % 148: FOP-ASSEMBLER-FIXUP offset(4) -> stack

    This operation resolves a reference to an assembler routine.  The stack args
    are (routine-name, kind and code-object).  The kind is a
    target-dependent symbol indicating the instruction format of the patch target
    (at offset bytes from the start of the code area.)  The code object is
    left on stack top (allowing multiple uses of this FOP to be chained.)

  % 149-199: Unused

  % 200: FOP-RPLACA table-idx(4) cdr-offset(4)

  % 201: FOP-RPLACD table-idx(4) cdr-offset(4)

    These operations destructively modify a list entered in the table.  {\it
    table-idx is the table entry holding the list, and cdr-offset designates
    the cons in the list to modify (like the argument to nthcdr.)  The new
    value is popped off of the stack, and stored in the car or cdr,
    respectively.

  % 202: FOP-SVSET table-idx(4) vector-idx(4)

    Destructively modifies a simple-vector entered in the table.  Pops the
    new value off of the stack, and stores it in the vector-idx element of
    the contents of the table entry table-idx.

  % 203: FOP-NTHCDR cdr-offset(4) -> stack

    Does nthcdr on the top-of stack, leaving the result there.

  % 204: FOP-STRUCTSET table-idx(4) vector-idx(4)

    Like FOP-SVSET, except it alters structure slots.

  % 255: FOP-END-HEADER Indicates the end of a group header,

    as described above.
]#
