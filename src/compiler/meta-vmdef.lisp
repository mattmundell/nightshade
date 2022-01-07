;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/meta-vmdef.lisp,v 1.7 1994/10/31 04:27:28 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the implementation-independent facilities used for
;;; defining the compiler's interface to the VM in a given implementation
;;; that are needed at meta-compile time.  They are seperated out from
;;; vmdef.lisp so that they can be compiled and loaded without trashing the
;;; running compiler.
;;;
;;; Written by Rob MacLachlan
;;; Seperated from vmdef.lisp by William Lott
;;;
(in-package :c)

(export '(define-storage-base define-storage-class define-move-function
	  define-move-function define-move-vop 
	  meta-primitive-type-or-lose
	  def-primitive-type def-primitive-type-alias
	  primitive-type-vop define-vop sc-case sc-is
	  note-this-location note-next-instruction))


;;;; Storage class and storage base definition:

;;; Define-Storage-Base  --  Public
;;;
;;;    Enter the basic structure at meta-compile time, and then fill in the
;;; missing slots at load time.
;;;
(defmacro define-storage-base (name kind &key size)
  "Define-Storage-Base Name Kind {Key Value}*
  Define a storage base having the specified Name.  Kind may be :Finite,
  :Unbounded or :Non-Packed.  The following keywords are legal:

  :Size <Size>
      Specify the number of locations in a :Finite SB or the initial size of a
      :Unbounded SB."
  (check-type name symbol)
  (check-type kind (member :finite :unbounded :non-packed))
  (ecase kind
    (:non-packed
     (when size
       (error "Size specification meaningless in a ~S SB." kind)))
    ((:finite :unbounded)
     (unless size (error "Size not specified in a ~S SB." kind))
     (check-type size unsigned-byte)))
    
  (let ((res (if (eq kind :non-packed)
		 (make-sb :name name :kind kind)
		 (make-finite-sb :name name :kind kind :size size))))
    `(progn
       (eval-when (compile load eval)
	 (setf (gethash ',name (backend-meta-sb-names *target-backend*))
	       ',res))
       ,(if (eq kind :non-packed)
	    `(setf (gethash ',name (backend-sb-names *target-backend*))
		   (copy-sb ',res))
	    `(let ((res (copy-finite-sb ',res)))
	       (setf (finite-sb-always-live res)
		     (make-array ',size :initial-element #*))
	       (setf (finite-sb-conflicts res)
		     (make-array ',size :initial-element '#()))
	       (setf (finite-sb-live-tns res)
		     (make-array ',size :initial-element nil))
	       (setf (gethash ',name (backend-sb-names *target-backend*))
		     res)))

       (setf (backend-sb-list *target-backend*)
	     (cons (sb-or-lose ',name)
		   (remove ',name (backend-sb-list *target-backend*)
			   :key #'sb-name)))
       ',name)))


;;; Define-Storage-Class  --  Public
;;;
;;;
(defmacro define-storage-class (name number sb-name &key (element-size '1)
				     (alignment '1) locations reserve-locations
				     save-p alternate-scs constant-scs)
  "Define-Storage-Class Name Number Storage-Base {Key Value}*
  Define a storage class Name that uses the named Storage-Base.  Number is a
  small, non-negative integer that is used as an alias.  The following
  keywords are defined:

  :Element-Size Size
      The size of objects in this SC in whatever units the SB uses.  This
      defaults to 1.

  :Alignment Size
      The alignment restrictions for this SC.  TNs will only be allocated at
      offsets that are an even multiple of this number.  Defaults to 1.

  :Locations (Location*)
      If the SB is :Finite, then this is a list of the offsets within the SB
      that are in this SC.

  :Reserve-Locations (Location*)
      A subset of the Locations that the register allocator should try to
      reserve for operand loading (instead of to hold variable values.)

  :Save-P {T | NIL}
      If T, then values stored in this SC must be saved in one of the
      non-save-p :Alternate-SCs across calls.

  :Alternate-SCs (SC*)
      Indicates other SCs that can be used to hold values from this SC across
      calls or when storage in this SC is exhausted.  The SCs should be
      specified in order of decreasing \"goodness\".  There must be at least
      one SC in an unbounded SB, unless this SC is only used for restricted or
      wired TNs.

  :Constant-SCs (SC*)
      A list of the names of all the constant SCs that can be loaded into this
      SC by a move function."
  
  (check-type name symbol)
  (check-type number sc-number)
  (check-type sb-name symbol)
  (check-type locations list)
  (check-type reserve-locations list)
  (check-type save-p boolean)
  (check-type alternate-scs list)
  (check-type constant-scs list)
  (unless (= (logcount alignment) 1)
    (error "Alignment is not a power of two: ~S" alignment))

  (let ((sb (meta-sb-or-lose sb-name)))
    (if (eq (sb-kind sb) :finite)
	(let ((size (sb-size sb))
	      (element-size (eval element-size)))
	  (check-type element-size unsigned-byte)
	  (dolist (el locations)
	    (check-type el unsigned-byte)
	    (unless (<= 1 (+ el element-size) size)
	      (error "SC element ~D out of bounds for ~S." el sb))))
	(when locations
	  (error ":Locations is meaningless in a ~S SB." (sb-kind sb))))

    (unless (subsetp reserve-locations locations)
      (error "Reserve-Locations not a subset of Locations."))

    (when (and (or alternate-scs constant-scs)
	       (eq (sb-kind sb) :non-packed))
      (error "Meaningless to specify alternate or constant SCs in a ~S SB."
	     (sb-kind sb))))

  (let ((nstack-p
	 (if (or (eq sb-name 'non-descriptor-stack)
		 (find 'non-descriptor-stack
		       (mapcar #'meta-sc-or-lose alternate-scs)
		       :key #'(lambda (x)
				(sb-name (sc-sb x)))))
	     t nil)))
    `(progn
       (eval-when (compile load eval)
	 (let ((res (make-sc :name ',name :number ',number
			     :sb (meta-sb-or-lose ',sb-name)
			     :element-size ,element-size
			     :alignment ,alignment
			     :locations ',locations
			     :reserve-locations ',reserve-locations
			     :save-p ',save-p
			     :number-stack-p ,nstack-p
			     :alternate-scs (mapcar #'meta-sc-or-lose
						    ',alternate-scs)
			     :constant-scs (mapcar #'meta-sc-or-lose
						   ',constant-scs))))
	   (setf (gethash ',name (backend-meta-sc-names *target-backend*)) res)
	   (setf (svref (backend-meta-sc-numbers *target-backend*) ',number)
		 res)
	   (setf (svref (sc-load-costs res) ',number) 0)))

       (let ((old (svref (backend-sc-numbers *target-backend*) ',number)))
	 (when (and old (not (eq (sc-name old) ',name)))
	   (warn "Redefining SC number ~D from ~S to ~S." ',number
		 (sc-name old) ',name)))
       
       (setf (svref (backend-sc-numbers *target-backend*) ',number)
	     (meta-sc-or-lose ',name))
       (setf (gethash ',name (backend-sc-names *target-backend*))
	     (meta-sc-or-lose ',name))
       (setf (sc-sb (sc-or-lose ',name)) (sb-or-lose ',sb-name))
       ',name)))


;;;; Move/coerce definition:

;;; DO-SC-PAIRS  --  Internal
;;;
;;;    Given a list of paris of lists of SCs (as given to DEFINE-MOVE-VOP,
;;; etc.), bind TO-SC and FROM-SC to all the combinations.
;;;
(defmacro do-sc-pairs ((from-sc-var to-sc-var scs) &body body)
  `(do ((froms ,scs (cddr froms))
	(tos (cdr ,scs) (cddr tos)))
       ((null froms))
     (dolist (from (car froms))
       (let ((,from-sc-var (meta-sc-or-lose from)))
	 (dolist (to (car tos))
	   (let ((,to-sc-var (meta-sc-or-lose to)))
	     ,@body))))))


;;; DEFINE-MOVE-FUNCTION  --  Public
;;;
(defmacro define-move-function ((name cost) lambda-list scs &body body)
  "Define-Move-Function (Name Cost) lambda-list ({(From-SC*) (To-SC*)}*) form*
  Define the function Name and note it as the function used for moving operands
  from the From-SCs to the To-SCs.  Cost is the cost of this move operation.
  The function is called with three arguments: the VOP (for context), and the
  source and destination TNs.  An ASSEMBLE form is wrapped around the body.
  All uses of DEFINE-MOVE-FUNCTION should be compiled before any uses of
  DEFINE-VOP."
  (when (or (oddp (length scs)) (null scs))
    (error "Malformed SCs spec: ~S." scs))
  (check-type cost index)
  `(progn
     (eval-when (compile load eval)
       (do-sc-pairs (from-sc to-sc ',scs)
	 (unless (eq from-sc to-sc)
	   (let ((num (sc-number from-sc)))
	     (setf (svref (sc-move-functions to-sc) num) ',name)
	     (setf (svref (sc-load-costs to-sc) num) ',cost)))))

     (defun ,name ,lambda-list
       (new-assem:assemble (*code-segment* ,(first lambda-list))
	 ,@body))))


(defconstant sc-vop-slots '((:move . sc-move-vops)
			    (:move-argument . sc-move-arg-vops)))

;;; DEFINE-MOVE-VOP  --  Public
;;;
;;;    We record the VOP and costs for all SCs that we can move between
;;; (including implicit loading).
;;;
(defmacro define-move-vop (name kind &rest scs)
  "Define-Move-VOP Name {:Move | :Move-Argument} {(From-SC*) (To-SC*)}*
  Make Name be the VOP used to move values in the specified From-SCs to the
  representation of the To-SCs.  If kind is :Move-Argument, then the VOP takes
  an extra argument, which is the frame pointer of the frame to move into." 
  (when (or (oddp (length scs)) (null scs))
    (error "Malformed SCs spec: ~S." scs))
  (let ((accessor (or (cdr (assoc kind sc-vop-slots))
		      (error "Unknown kind ~S." kind))))
    `(progn
       ,@(when (eq kind :move)
	   `((eval-when (compile load eval)
	       (do-sc-pairs (from-sc to-sc ',scs)
		 (compute-move-costs from-sc to-sc
				     ,(vop-parse-cost
				       (vop-parse-or-lose name)))))))
       
       (let ((vop (template-or-lose ',name)))
	 (do-sc-pairs (from-sc to-sc ',scs)
	   (dolist (dest-sc (cons to-sc (sc-alternate-scs to-sc)))
	     (let ((vec (,accessor dest-sc)))
	       (let ((scn (sc-number from-sc)))
		 (setf (svref vec scn)
		       (adjoin-template vop (svref vec scn))))
	       (dolist (sc (append (sc-alternate-scs from-sc)
				   (sc-constant-scs from-sc)))
		 (let ((scn (sc-number sc)))
		   (setf (svref vec scn)
			 (adjoin-template vop (svref vec scn))))))))))))


;;;; Primitive type definition:

;;; META-PRIMITIVE-TYPE-OR-LOSE  --  Interface
;;;
(defun meta-primitive-type-or-lose (name)
  (the primitive-type
       (or (gethash name (backend-meta-primitive-type-names *target-backend*))
	   (error "~S is not a defined primitive type." name))))

;;; Def-Primitive-Type  --  Public
;;;
;;;    If the primitive-type structure already exists, we destructively modify
;;; it so that existing references in templates won't be invalidated.
;;; Primitive-type definition isn't done at meta-compile time, so this doesn't
;;; break the running compiler.
;;;
(defmacro def-primitive-type (name scs &key (type name))
  "Def-Primitive-Type Name (SC*) {Key Value}*
   Define a primitive type Name.  Each SC specifies a Storage Class that values
   of this type may be allocated in.  The following keyword options are
   defined:
  
  :Type
      The type descriptor for the Lisp type that is equivalent to this type
      (defaults to Name.)"
  (check-type name symbol)
  (check-type scs list)
  (let ((scns (mapcar #'meta-sc-number-or-lose scs))
	(get-type `(specifier-type ',type)))
    `(progn
       (eval-when (compile load eval)
	 (setf (gethash ',name (backend-meta-primitive-type-names
				*target-backend*))
	       (make-primitive-type :name ',name  :scs ',scns
				    :type ,get-type)))
       ,(once-only ((n-old `(gethash ',name
				     (backend-primitive-type-names
				      *target-backend*)))
		    (n-type get-type))
	  `(progn
	     (cond (,n-old
		    (setf (primitive-type-scs ,n-old) ',scns)
		    (setf (primitive-type-type ,n-old) ,n-type))
		   (t
		    (setf (gethash ',name
				   (backend-primitive-type-names
				    *target-backend*))
			  (make-primitive-type :name ',name  :scs ',scns
					       :type ,n-type))))
	     ',name)))))

;;; Def-Primitive-Type-Alias  --  Public
;;;
;;; Just record the translation.
;;; 
(defmacro def-primitive-type-alias (name result)
  "DEF-PRIMITIVE-TYPE-ALIAS Name Result
  Define name to be an alias for Result in VOP operand type restrictions."
  `(eval-when (compile load eval)
     (setf (gethash ',name (backend-primitive-type-aliases *target-backend*))
	   ',result)
     ',name))

(defparameter primitive-type-slot-alist
  '((:check . primitive-type-check)))

;;; Primitive-Type-Vop  --  Public
;;;
(defmacro primitive-type-vop (vop kinds &rest types)
  "Primitive-Type-VOP Vop (Kind*) Type*
  Annotate all the specified primitive Types with the named VOP under each of
  the specified kinds:

  :Check
      A one argument one result VOP that moves the argument to the result,
      checking that the value is of this type in the process."
  (let ((n-vop (gensym))
	(n-type (gensym)))
    `(let ((,n-vop (template-or-lose ',vop)))
       ,@(mapcar
	  #'(lambda (type)
	      `(let ((,n-type (primitive-type-or-lose ',type)))
		 ,@(mapcar
		    #'(lambda (kind)
			(let ((slot (or (cdr (assoc kind
						    primitive-type-slot-alist))
					(error "Unknown kind: ~S." kind))))
			  `(setf (,slot ,n-type) ,n-vop)))
		    kinds)))
	  types)
       nil)))

;;; SC-ALLOWED-BY-PRIMITIVE-TYPE  --  Interface
;;;
;;;    Return true if SC is either one of Ptype's SC's, or one of those SC's
;;; alternate or constant SCs.
;;;
(defun meta-sc-allowed-by-primitive-type (sc ptype)
  (declare (type sc sc) (type primitive-type ptype))
  (let ((scn (sc-number sc)))
    (dolist (allowed (primitive-type-scs ptype) nil)
      (when (eql allowed scn)
	(return t))
      (let ((allowed-sc (svref (backend-meta-sc-numbers *target-backend*)
			       allowed)))
	(when (or (member sc (sc-alternate-scs allowed-sc))
		  (member sc (sc-constant-scs allowed-sc)))
	  (return t))))))



;;;; VOP definition structures:
;;;
;;;    Define-VOP uses some fairly complex data structures at meta-compile
;;; time, both to hold the results of parsing the elaborate syntax and to
;;; retain the information so that it can be inherited by other VOPs.

;;; The VOP-Parse structure holds everything we need to know about a VOP at
;;; meta-compile time.
;;;
(defstruct (vop-parse
	    (:print-function %print-vop-parse)
	    (:make-load-form-fun :just-dump-it-normally)
	    (:pure t))
  ;;
  ;; The name of this VOP.
  (name nil :type symbol)
  ;;
  ;; If true, then the name of the VOP we inherit from.
  (inherits nil :type (or symbol null))
  ;;
  ;; Lists of Operand-Parse structures describing the arguments, results and
  ;; temporaries of the VOP.
  (args nil :type list)
  (results nil :type list)
  (temps nil :type list)
  ;;
  ;; Operand-Parse structures containing information about more args and
  ;; results.  If null, then there there are no more operands of that kind.
  (more-args nil :type (or operand-parse null))
  (more-results nil :type (or operand-parse null))
  ;;
  ;; A list of all the above together.
  (operands nil :type list)
  ;;
  ;; Names of variables that should be declared ignore.
  (ignores () :type list)
  ;;
  ;; True if this is a :Conditional VOP.
  (conditional-p nil)
  ;;
  ;; Argument and result primitive types.  These are pulled out of the
  ;; operands, since we often want to change them without respecifying the
  ;; operands.
  (arg-types :unspecified :type (or (member :unspecified) list))
  (result-types :unspecified :type (or (member :unspecified) list))
  ;;
  ;; The guard expression specified, or NIL if none.
  (guard nil)
  ;;
  ;; The cost of and body code for the generator.
  (cost 0 :type unsigned-byte)
  (body :unspecified :type (or (member :unspecified) list))
  ;;
  ;; Info for VOP variants.  The list of forms to be evaluated to get the
  ;; variant args for this VOP, and the list of variables to be bound to the
  ;; variant args.
  (variant () :type list)
  (variant-vars () :type list)
  ;;
  ;; Variables bound to the VOP and Vop-Node when in the generator body.
  (vop-var (gensym) :type symbol)
  (node-var nil :type (or symbol null))
  ;;
  ;; A list of the names of the codegen-info arguments to this VOP.
  (info-args () :type list)
  ;;
  ;; An efficiency note associated with this VOP.
  (note nil :type (or string null))
  ;;
  ;; A list of the names of the Effects and Affected attributes for this VOP.
  (effects '(any) :type list)
  (affected '(any) :type list)
  ;;
  ;; A list of the names of functions this VOP is a translation of and the
  ;; policy that allows this translation to be done.  :Fast is a safe default,
  ;; since it isn't a safe policy.
  (translate () :type list)
  (policy :fast :type policies)
  ;;
  ;; Stuff used by life analysis.
  (save-p nil :type (member t nil :compute-only :force-to-stack))
  ;;
  ;; Info about how to emit move-argument VOPs for the more operand in
  ;; call/return VOPs.
  (move-args nil :type (member nil :local-call :full-call :known-return)))


(defprinter vop-parse
  name
  (inherits :test inherits)
  args
  results
  temps
  (more-args :test more-args)
  (more-results :test more-results)
  (conditional-p :test conditional-p)
  ignores
  arg-types
  result-types
  cost
  body
  (variant :test variant)
  (variant-vars :test variant-vars)
  (info-args :test info-args)
  (note :test note)
  effects
  affected
  translate
  policy
  (save-p :test save-p)
  (move-args :test move-args))

;;; The Operand-Parse structure contains stuff we need to know about and
;;; operand or temporary at meta-compile time.  Besides the obvious stuff, we
;;; also store the names of per-operand temporaries here.
;;;
(defstruct (operand-parse
	    (:print-function %print-operand-parse)
	    (:make-load-form-fun :just-dump-it-normally)
	    (:pure t))
  ;;
  ;; Name of the operand (which we bind to the TN).
  (name nil :type symbol)
  ;;
  ;; The way this operand is used:
  (kind (required-argument)
	:type (member :argument :result :temporary
		      :more-argument :more-result))
  ;;
  ;; If true, the name of an operand that this operand is targeted to.  This is
  ;; only meaningful in :Argument and :Temporary operands.
  (target nil :type (or symbol null))
  ;;
  ;; Temporary that holds the TN-Ref for this operand.  Temp-Temp holds the
  ;; write reference that begins a temporary's lifetime.
  (temp (gensym) :type symbol)
  (temp-temp nil :type (or symbol null))
  ;;
  ;; The time that this operand is first live and the time at which it becomes
  ;; dead again.  These are time-specs, as returned by parse-time-spec. 
  born
  dies
  ;;
  ;; A list of the names of the SCs that this operand is allowed into.  If
  ;; false, there is no restriction.
  (scs nil :type list)
  ;;
  ;; Variable that is bound to the load TN allocated for this operand, or to
  ;; NIL if no load-TN was allocated.
  (load-tn (gensym) :type symbol)
  ;;
  ;; An expression that tests whether to do automatic operand loading.
  (load t)
  ;;
  ;; In a wired or restricted temporary this is the SC the TN is to be packed
  ;; in.  Null otherwise.
  (sc nil :type (or symbol null))
  ;;
  ;; If non-null, we are a temp wired to this offset in SC.
  (offset nil :type (or unsigned-byte null)))


(defprinter operand-parse
  name
  kind
  (target :test target)
  born
  dies
  (scs :test scs)
  (load :test load)
  (sc :test sc)
  (offset :test offset))


;;;; Random utilities:

;;; Find-Operand  --  Internal
;;;
;;;    Find the operand or temporary with the specifed Name in the VOP Parse.
;;; If there is no such operand, signal an error.  Also error if the operand
;;; kind isn't one of the specified Kinds.  If Error-P is NIL, just return NIL
;;; if there is no such operand.
;;;
(defun find-operand (name parse &optional
			  (kinds '(:argument :result :temporary))
			  (error-p t))
  (declare (symbol name) (type vop-parse parse) (list kinds))
  (let ((found (find name (vop-parse-operands parse)
		     :key #'operand-parse-name)))
    (if found
	(unless (member (operand-parse-kind found) kinds)
	  (error "Operand ~S isn't one of these kinds: ~S." name kinds))
	(when error-p
	  (error "~S is not an operand to ~S." name (vop-parse-name parse))))
    found))


;;; VOP-Parse-Or-Lose  --  Internal
;;;
;;;    Get the VOP-Parse structure for Name or die trying.  For all
;;; meta-compile time uses, the VOP-Parse should be used instead of the
;;; VOP-Info
;;;
(defun vop-parse-or-lose (name &optional (backend *target-backend*))
  (the vop-parse
       (or (gethash name (backend-parsed-vops backend))
	   (error "~S is not the name of a defined VOP." name))))


;;; Access-Operands  --  Internal
;;;
;;;    Return a list of let-forms to parse a tn-ref list into a the temps
;;; specified by the operand-parse structures.  More-Operand is the
;;; Operand-Parse describing any more operand, or NIL if none.  Refs is an
;;; expression that evaluates into the first tn-ref.
;;;
(defun access-operands (operands more-operand refs)
  (declare (list operands))
  (collect ((res))
    (let ((prev refs))
      (dolist (op operands)
	(let ((n-ref (operand-parse-temp op)))
	  (res `(,n-ref ,prev))
	  (setq prev `(tn-ref-across ,n-ref))))

      (when more-operand
	(res `(,(operand-parse-name more-operand) ,prev))))
    (res)))


;;; Ignore-Unreferenced-Temps --  Internal
;;;
;;;    Used with Access-Operands to prevent warnings for TN-Ref temps not used
;;; by some particular function.  It returns the name of the last operand, or
;;; NIL if Operands is NIL.
;;;
(defun ignore-unreferenced-temps (operands)
  (when operands
    (operand-parse-temp (car (last operands)))))


;;; VOP-Spec-Arg  --  Internal
;;;
;;;    Grab an arg out of a VOP spec, checking the type and syntax and stuff.
;;;
(defun vop-spec-arg (spec type &optional (n 1) (last t))
  (let ((len (length spec)))
    (when (<= len n)
      (error "~:R argument missing: ~S." n spec))
    (when (and last (> len (1+ n)))
      (error "Extra junk at end of ~S." spec))
    (let ((thing (elt spec n)))
      (unless (typep thing type)
	(error "~:R argument is not a ~S: ~S." n type spec))
      thing)))


;;;; Time specs:

;;; Parse-Time-Spec  --  Internal
;;;
;;;    Return a time spec describing a time during the evaluation of a VOP,
;;; used to delimit operand and temporary lifetimes.  The representation is a
;;; cons whose CAR is the number of the evaluation phase and the CDR is the
;;; sub-phase.  The sub-phase is 0 in the :Load and :Save phases. 
;;;
(defun parse-time-spec (spec)
  (let ((dspec (if (atom spec) (list spec 0) spec)))
    (unless (and (= (length dspec) 2)
		 (typep (second dspec) 'unsigned-byte))
      (error "Malformed time specifier: ~S." spec))

    (cons (case (first dspec)
	    (:load 0)
	    (:argument 1)
	    (:eval 2)
	    (:result 3)
	    (:save 4)
	    (t
	     (error "Unknown phase in time specifier: ~S." spec)))
	  (second dspec))))


;;; Time-Spec-Order  --  Internal
;;;
;;;    Return true if the time spec X is the same or later time than Y.
;;;
(defun time-spec-order (x y)
  (or (> (car x) (car y))
      (and (= (car x) (car y))
	   (>= (cdr x) (cdr y)))))
 

;;;; Emit function generation:

(defun compute-temporaries-description (parse)
  (let ((temps (vop-parse-temps parse)))
    (when temps
      (let ((results (make-array (length temps)
				 :element-type '(unsigned-byte 16)))
	    (index 0))
	(dolist (temp temps)
	  (declare (type operand-parse temp))
	  (let ((sc (operand-parse-sc temp))
		(offset (operand-parse-offset temp)))
	    (assert sc)
	    (setf (aref results index)
		  (if offset
		      (+ (ash offset (1+ sc-bits))
			 (ash (meta-sc-number-or-lose sc) 1)
			 1)
		      (ash (meta-sc-number-or-lose sc) 1))))
	  (incf index))
	results))))

(defun compute-ref-ordering (parse)
  (let* ((num-args (+ (length (vop-parse-args parse))
		      (if (vop-parse-more-args parse) 1 0)))
	 (num-results (+ (length (vop-parse-results parse))
			 (if (vop-parse-more-results parse) 1 0)))
	 (index 0))
    (collect ((refs) (targets))
      (dolist (op (vop-parse-operands parse))
	(when (operand-parse-target op)
	  (unless (member (operand-parse-kind op) '(:argument :temporary))
	    (error "Cannot target a ~S operand: ~S." (operand-parse-kind op)
		   (operand-parse-name op)))
	  (let ((target (find-operand (operand-parse-target op) parse
				      '(:temporary :result))))
	    (targets (+ (* index max-vop-tn-refs)
			(ecase (operand-parse-kind target)
			  (:result
			   (+ (eposition target (vop-parse-results parse))
			      num-args))
			  (:temporary
			   (+ (* (eposition target (vop-parse-temps parse)) 2)
			      num-args num-results)))))))
	(let ((born (operand-parse-born op))
	      (dies (operand-parse-dies op)))
	  (ecase (operand-parse-kind op)
	    (:argument
	     (refs (cons (cons dies nil) index)))
	    (:more-argument
	     (refs (cons (cons dies nil) index)))
	    (:result
	     (refs (cons (cons born t) index)))
	    (:more-result
	     (refs (cons (cons born t) index)))
	    (:temporary
	     (refs (cons (cons dies nil) index))
	     (incf index)
	     (refs (cons (cons born t) index))))
	  (incf index)))
      (let* ((sorted (sort (refs)
			   #'(lambda (x y)
			       (let ((x-time (car x))
				     (y-time (car y)))
				 (if (time-spec-order x-time y-time)
				     (if (time-spec-order y-time x-time)
					 (and (not (cdr x)) (cdr y))
					 nil)
				     t)))
			   :key #'car))
	     (ordering (make-array (length sorted)
				   :element-type '(mod #.max-vop-tn-refs))))
	(let ((index 0))
	  (dolist (ref sorted)
	    (setf (aref ordering index) (cdr ref))
	    (incf index)))
	`(:num-args ,num-args
	  :num-results ,num-results
	  :ref-ordering ,ordering
	  ,@(when (targets)
	      `(:targets
		,(make-array (length (targets))
			     :element-type '(mod #.(* max-vop-tn-refs 2))
			     :initial-contents (targets)))))))))

(defun make-emit-function-and-friends (parse)
  `(:emit-function #'emit-generic-vop
    :temps ',(compute-temporaries-description parse)
    ,@(compute-ref-ordering parse)))


;;;; Generator functions:

;;; FIND-MOVE-FUNCTIONS  --  Internal
;;;
;;;    Return an alist that translates from lists of SCs we can load OP from to
;;; the move function used for loading those SCs.  We quietly ignore
;;; restrictions to :non-packed (constant) and :unbounded SCs, since we don't
;;; load into those SCs.
;;;
(defun find-move-functions (op load-p)
  (collect ((funs))
    (dolist (sc-name (operand-parse-scs op))
      (let* ((sc (meta-sc-or-lose sc-name))
	     (scn (sc-number sc))
	     (load-scs (append (when load-p
				 (sc-constant-scs sc))
			       (sc-alternate-scs sc))))
	(cond
	 (load-scs
	  (dolist (alt load-scs)
	    (unless (member (sc-name alt) (operand-parse-scs op) :test #'eq)
	      (let* ((altn (sc-number alt))
		     (name (if load-p
			       (svref (sc-move-functions sc) altn)
			       (svref (sc-move-functions alt) scn)))
		     (found (or (assoc alt (funs) :test #'member)
				(rassoc name (funs)))))
		(unless name
		  (error "No move function defined to ~:[save~;load~] SC ~S~
			  ~:[to~;from~] from SC ~S."
			 load-p sc-name load-p (sc-name alt)))
		
		(cond (found
		       (unless (eq (cdr found) name)
			 (error "Can't tell whether to ~:[save~;load~] with ~S~@
				 or ~S when operand is in SC ~S."
				load-p name (cdr found) (sc-name alt)))
		       (pushnew alt (car found)))
		      (t
		       (funs (cons (list alt) name))))))))
	 ((member (sb-kind (sc-sb sc)) '(:non-packed :unbounded)))
	 (t
	  (error "SC ~S has no alternate~:[~; or constant~] SCs, yet it is~@
	          mentioned in the restriction for operand ~S."
		 sc-name load-p (operand-parse-name op))))))
    (funs)))

;;; CALL-MOVE-FUNCTION  --  Internal
;;;
;;;    Return a form to load/save the specified operand when it has a load TN.
;;; For any given SC that we can load from, there must be a unique load
;;; function.  If all SCs we can load from have the same move function, then we
;;; just call that when there is a load TN.  If there are multiple possible
;;; move functions, then we dispatch off of the operand TN's type to see which
;;; move function to use.
;;;
(defun call-move-function (parse op load-p)
  (let ((funs (find-move-functions op load-p))
	(load-tn (operand-parse-load-tn op)))
    (if funs
	(let* ((tn `(tn-ref-tn ,(operand-parse-temp op)))
	       (n-vop (or (vop-parse-vop-var parse)
			  (setf (vop-parse-vop-var parse) (gensym))))
	       (form (if (rest funs)
			 `(sc-case ,tn
			    ,@(mapcar #'(lambda (x)
					  `(,(mapcar #'sc-name (car x))
					    ,(if load-p
						 `(,(cdr x) ,n-vop ,tn
						   ,load-tn)
						 `(,(cdr x) ,n-vop ,load-tn
						   ,tn))))
				      funs))
			 (if load-p
			     `(,(cdr (first funs)) ,n-vop ,tn ,load-tn)
			     `(,(cdr (first funs)) ,n-vop ,load-tn ,tn)))))
	  (if (eq (operand-parse-load op) t)
	      `(when ,load-tn ,form)
	      `(when (eq ,load-tn ,(operand-parse-name op))
		 ,form)))
	`(when ,load-tn
	   (error "Load TN allocated, but no move function?~@
	           VM definition inconsistent, recompile and try again.")))))

;;; DECIDE-TO-LOAD  --  Internal
;;;
;;;    Return the TN that we should bind to the operand's var in the generator
;;; body.  In general, this involves evaluating the :LOAD-IF test expression.
;;;
(defun decide-to-load (parse op)
  (let ((load (operand-parse-load op))
	(load-tn (operand-parse-load-tn op))
	(temp (operand-parse-temp op)))
    (if (eq load t)
	`(or ,load-tn (tn-ref-tn ,temp))
	(collect ((binds)
		  (ignores))
	  (dolist (x (vop-parse-operands parse))
	    (when (member (operand-parse-kind x) '(:argument :result))
	      (let ((name (operand-parse-name x)))
		(binds `(,name (tn-ref-tn ,(operand-parse-temp x))))
		(ignores name))))
	  `(if (and ,load-tn
		    (let ,(binds)
		      #+new-compiler
		      (declare (ignorable ,@(ignores)))
		      #-new-compiler
		      (progn ,@(ignores))
		      ,load))
	       ,load-tn
	       (tn-ref-tn ,temp))))))

;;; Make-Generator-Function  --  Internal
;;;
;;;    Make a lambda that parses the VOP TN-Refs, does automatic operand
;;; loading, and runs the appropriate code generator.
;;;
(defun make-generator-function (parse)
  (declare (type vop-parse parse))
  (let ((n-vop (vop-parse-vop-var parse))
	(operands (vop-parse-operands parse))
	(n-info (gensym)) (n-variant (gensym)))
    (collect ((binds)
	      (loads)
	      (saves))
      (dolist (op operands)
	(ecase (operand-parse-kind op)
	  ((:argument :result)
	   (let ((temp (operand-parse-temp op))
		 (name (operand-parse-name op)))
	     (cond ((and (operand-parse-load op) (operand-parse-scs op))
		    (binds `(,(operand-parse-load-tn op)
			     (tn-ref-load-tn ,temp)))
		    (binds `(,name ,(decide-to-load parse op)))
		    (if (eq (operand-parse-kind op) :argument)
			(loads (call-move-function parse op t))
			(saves (call-move-function parse op nil))))
		   (t
		    (binds `(,name (tn-ref-tn ,temp)))))))
	  (:temporary
	   (binds `(,(operand-parse-name op)
		    (tn-ref-tn ,(operand-parse-temp op)))))
	  ((:more-argument :more-result))))

      `#'(lambda (,n-vop)
	   (let* (,@(access-operands (vop-parse-args parse)
				     (vop-parse-more-args parse)
				     `(vop-args ,n-vop))
		  ,@(access-operands (vop-parse-results parse)
				     (vop-parse-more-results parse)
				     `(vop-results ,n-vop))
		  ,@(access-operands (vop-parse-temps parse) nil
				     `(vop-temps ,n-vop))
		  ,@(when (vop-parse-info-args parse)
		      `((,n-info (vop-codegen-info ,n-vop))
			,@(mapcar #'(lambda (x) `(,x (pop ,n-info)))
				  (vop-parse-info-args parse))))
		  ,@(when (vop-parse-variant-vars parse)
		      `((,n-variant (vop-info-variant (vop-info ,n-vop)))
			,@(mapcar #'(lambda (x) `(,x (pop ,n-variant)))
				  (vop-parse-variant-vars parse))))
		  ,@(when (vop-parse-node-var parse)
		      `((,(vop-parse-node-var parse) (vop-node ,n-vop))))
		  ,@(binds))
	     (declare (ignore ,@(vop-parse-ignores parse)))
	     ,@(loads)
	     (new-assem:assemble (*code-segment* ,n-vop)
	       ,@(vop-parse-body parse))
	     ,@(saves))))))


;;; Parse-Operands  --  Internal
;;;
;;;    Given a list of operand specifications as given to Define-VOP, return a
;;; list of Operand-Parse structures describing the fixed operands, and a
;;; single Operand-Parse describing any more operand.  If we are inheriting a
;;; VOP, we default attributes to the inherited operand of the same name.
;;;
(defun parse-operands (parse specs kind)
  (declare (list specs)
	   (type (member :argument :result) kind))
  (let ((num -1)
	(more nil))
    (collect ((operands))
      (dolist (spec specs)
	(unless (and (consp spec) (symbolp (first spec)) (oddp (length spec)))
	  (error "Malformed operand specifier: ~S." spec))
	(when more
	  (error "More operand isn't last: ~S." specs)) 
	(let* ((name (first spec))
	       (old (if (vop-parse-inherits parse)
			(find-operand name
				      (vop-parse-or-lose
				       (vop-parse-inherits parse))
				      (list kind)
				      nil)
			nil))
	       (res (if old
			(make-operand-parse
			 :name name
			 :kind kind
			 :target (operand-parse-target old)
			 :born (operand-parse-born old)
			 :dies (operand-parse-dies old)
			 :scs (operand-parse-scs old)
			 :load-tn (operand-parse-load-tn old)
			 :load (operand-parse-load old))
			(ecase kind
			  (:argument
			   (make-operand-parse
			    :name (first spec)  :kind :argument
			    :born (parse-time-spec :load)
			    :dies (parse-time-spec `(:argument ,(incf num)))))
			  (:result
			   (make-operand-parse
			    :name (first spec)  :kind :result
			    :born (parse-time-spec `(:result ,(incf num)))
			    :dies (parse-time-spec :save)))))))
	  (do ((key (rest spec) (cddr key)))
	      ((null key))
	    (let ((value (second key)))
	      (case (first key)
		(:scs
		 (check-type value list)
		 (setf (operand-parse-scs res) (remove-duplicates value)))
		(:load-tn
		 (check-type value symbol)
		 (setf (operand-parse-load-tn res) value))
		(:load-if
		 (setf (operand-parse-load res) value))
		(:more
		 (check-type value boolean)
		 (setf (operand-parse-kind res)
		       (if (eq kind :argument) :more-argument :more-result))
		 (setf (operand-parse-load res) nil)
		 (setq more res))
		(:target
		 (check-type value symbol)
		 (setf (operand-parse-target res) value))
		(:from
		 (unless (eq kind :result)
		   (error "Can only specify :FROM in a result: ~S" spec))
		 (setf (operand-parse-born res) (parse-time-spec value)))
		(:to
		 (unless (eq kind :argument)
		   (error "Can only specify :TO in an argument: ~S" spec))
		 (setf (operand-parse-dies res) (parse-time-spec value)))
		(t
		 (error "Unknown keyword in operand specifier: ~S." spec)))))

	  (cond ((not more)
		 (operands res))
		((operand-parse-target more)
		 (error "Cannot specify :TARGET in a :MORE operand."))
		((operand-parse-load more)
		 (error "Cannot specify :LOAD-IF in a :MORE operand.")))))
      (values (the list (operands)) more))))


;;; Parse-Temporary  --  Internal
;;;
;;;    Parse a temporary specification, entering the Operand-Parse structures
;;; in the Parse structure.
;;;
(defun parse-temporary (spec parse)
  (declare (list spec)
	   (type vop-parse parse))
  (let ((len (length spec)))
    (unless (>= len 2)
      (error "Malformed temporary spec: ~S." spec))
    (unless (listp (second spec))
      (error "Malformed options list: ~S." (second spec)))
    (unless (evenp (length (second spec)))
      (error "Odd number of arguments in keyword options: ~S." spec))
    (unless (consp (cddr spec))
      (warn "Temporary spec allocates no temps:~%  ~S" spec))
    (dolist (name (cddr spec))
      (unless (symbolp name)
	(error "Bad temporary name: ~S." name))
      (let ((res (make-operand-parse :name name  :kind :temporary
				     :temp-temp (gensym)
				     :born (parse-time-spec :load)
				     :dies (parse-time-spec :save))))
	(do ((opt (second spec) (cddr opt)))
	    ((null opt))
	  (case (first opt)
	    (:target
	     (setf (operand-parse-target res)
		   (vop-spec-arg opt 'symbol 1 nil)))
	    (:sc
	     (setf (operand-parse-sc res)
		   (vop-spec-arg opt 'symbol 1 nil)))
	    (:offset
	     (let ((offset (eval (second opt))))
	       (check-type offset unsigned-byte)
	       (setf (operand-parse-offset res) offset)))
	    (:from
	     (setf (operand-parse-born res) (parse-time-spec (second opt))))
	    (:to
	     (setf (operand-parse-dies res) (parse-time-spec (second opt))))
	    ;;
	    ;; Backward compatibility...
	    (:scs
	     (let ((scs (vop-spec-arg opt 'list 1 nil)))
	       (unless (= (length scs) 1)
		 (error "Must specify exactly one SC for a temporary."))
	       (setf (operand-parse-sc res) (first scs))))
	    (:type)
	    (t
	     (error "Unknown temporary option: ~S." opt))))

	(unless (and (time-spec-order (operand-parse-dies res)
				      (operand-parse-born res))
		     (not (time-spec-order (operand-parse-born res)
					   (operand-parse-dies res))))
	  (error "Temporary lifetime doesn't begin before it ends: ~S." spec))

	(unless (operand-parse-sc res)
	  (error "Must specifiy :SC for all temporaries: ~S" spec))

	(setf (vop-parse-temps parse)
	      (cons res
		    (remove name (vop-parse-temps parse)
			    :key #'operand-parse-name))))))
  (undefined-value))


;;; Parse-Define-VOP  --  Internal
;;;
;;;    Top-level parse function.  Clobber Parse to represent the specified
;;; options.
;;;
(defun parse-define-vop (parse specs)
  (declare (type vop-parse parse) (list specs))
  (dolist (spec specs)
    (unless (consp spec)
      (error "Malformed option specification: ~S." spec))
    (case (first spec)
      (:args
       (multiple-value-bind
	   (fixed more)
	   (parse-operands parse (rest spec) :argument)
	 (setf (vop-parse-args parse) fixed)
	 (setf (vop-parse-more-args parse) more)))
      (:results
       (multiple-value-bind
	   (fixed more)
	   (parse-operands parse (rest spec) :result)
	 (setf (vop-parse-results parse) fixed)
	 (setf (vop-parse-more-results parse) more))
       (setf (vop-parse-conditional-p parse) nil))
      (:conditional
       (setf (vop-parse-result-types parse) ())
       (setf (vop-parse-results parse) ())
       (setf (vop-parse-more-results parse) nil)
       (setf (vop-parse-conditional-p parse) t))
      (:temporary
       (parse-temporary spec parse))
      (:generator
       (setf (vop-parse-cost parse)
	     (vop-spec-arg spec 'unsigned-byte 1 nil))
       (setf (vop-parse-body parse) (cddr spec)))
      (:effects
       (setf (vop-parse-effects parse) (rest spec)))
      (:affected
       (setf (vop-parse-affected parse) (rest spec)))
      (:info
       (setf (vop-parse-info-args parse) (rest spec)))
      (:ignore
       (setf (vop-parse-ignores parse) (rest spec)))
      (:variant
       (setf (vop-parse-variant parse) (rest spec)))
      (:variant-vars
       (let ((vars (rest spec)))
	 (setf (vop-parse-variant-vars parse) vars)
	 (setf (vop-parse-variant parse)
	       (make-list (length vars) :initial-element nil))))
      (:variant-cost
       (setf (vop-parse-cost parse) (vop-spec-arg spec 'unsigned-byte)))
      (:vop-var
       (setf (vop-parse-vop-var parse) (vop-spec-arg spec 'symbol)))
      (:move-args
       (setf (vop-parse-move-args parse)
	     (vop-spec-arg spec '(member nil :local-call :full-call
					 :known-return))))
      (:node-var
       (setf (vop-parse-node-var parse) (vop-spec-arg spec 'symbol)))
      (:note
       (setf (vop-parse-note parse) (vop-spec-arg spec '(or string null))))
      (:arg-types
       (setf (vop-parse-arg-types parse)
	     (parse-operand-types (rest spec) t)))
      (:result-types
       (setf (vop-parse-result-types parse)
	     (parse-operand-types (rest spec) nil)))
      (:translate
       (setf (vop-parse-translate parse) (rest spec)))
      (:guard
       (setf (vop-parse-guard parse) (vop-spec-arg spec t)))
      (:policy
       (setf (vop-parse-policy parse) (vop-spec-arg spec 'policies)))
      (:save-p
       (setf (vop-parse-save-p parse)
	     (vop-spec-arg spec
			   '(member t nil :compute-only :force-to-stack))))
      (t
       (error "Unknown option specifier: ~S." (first spec)))))
  (undefined-value))


;;;; Make costs and restrictions:

;;; Compute-Loading-Costs  --  Internal
;;;
;;; Given an operand, returns two values:
;;; 1] A SC-vector of the cost for the operand being in that SC, including both
;;;    the costs for move functions and coercion VOPs.
;;; 2] A SC-vector holding the SC that we load into, for any SC that we can
;;;    directly load from.
;;;
;;; In both vectors, unused entries are NIL.  Load-P specifies the direction:
;;; if true, we are loading, if false we are saving.
;;;
(defun compute-loading-costs (op load-p)
  (declare (type operand-parse op))
  (let ((scs (operand-parse-scs op))
	(costs (make-array sc-number-limit :initial-element nil))
	(load-scs (make-array sc-number-limit :initial-element nil)))
    (dolist (sc-name scs)
      (let* ((load-sc (meta-sc-or-lose sc-name))
	     (load-scn (sc-number load-sc)))
	(setf (svref costs load-scn) 0)
	(setf (svref load-scs load-scn) t)
	(dolist (op-sc (append (when load-p
				 (sc-constant-scs load-sc))
			       (sc-alternate-scs load-sc)))
	  (let* ((op-scn (sc-number op-sc))
		 (load (if load-p
			   (aref (sc-load-costs load-sc) op-scn)
			   (aref (sc-load-costs op-sc) load-scn))))
	    (unless load
	      (error "No move function defined to move ~:[from~;to~] SC ~
	              ~S~%~:[to~;from~] alternate or constant SC ~S."
		     load-p sc-name load-p (sc-name op-sc)))
	    
	    (let ((op-cost (svref costs op-scn)))
	      (when (or (not op-cost) (< load op-cost))
		(setf (svref costs op-scn) load)))

	    (let ((op-load (svref load-scs op-scn)))
	      (unless (eq op-load t)
		(pushnew load-scn (svref load-scs op-scn))))))

	(dotimes (i sc-number-limit)
	  (unless (svref costs i)
	    (let ((op-sc (svref (backend-meta-sc-numbers *target-backend*) i)))
	      (when op-sc
		(let ((cost (if load-p
				(svref (sc-move-costs load-sc) i)
				(svref (sc-move-costs op-sc) load-scn))))
		  (when cost
		    (setf (svref costs i) cost)))))))))

    (values costs load-scs)))

(defparameter no-costs
  (make-array sc-number-limit  :initial-element 0))

(defparameter no-loads
  (make-array sc-number-limit :initial-element 't))


;;; COMPUTE-LOADING-COSTS-IF-ANY  --  Internal
;;;
;;;    Pick off the case of operands with no restrictions.
;;;
(defun compute-loading-costs-if-any (op load-p)
  (declare (type operand-parse op))
  (if (operand-parse-scs op)
      (compute-loading-costs op load-p)
      (values no-costs no-loads)))

;;; COMPUTE-COSTS-AND-RESTRICTIONS-LIST  --  Internal
;;;
(defun compute-costs-and-restrictions-list (ops load-p)
  (declare (list ops))
  (collect ((costs)
	    (scs))
    (dolist (op ops)
      (multiple-value-bind (costs scs)
			   (compute-loading-costs-if-any op load-p)
	(costs costs)
	(scs scs)))
    (values (costs) (scs))))

;;; Make-Costs-And-Restrictions  --  Internal
;;;
(defun make-costs-and-restrictions (parse)
  (multiple-value-bind
      (arg-costs arg-scs)
      (compute-costs-and-restrictions-list (vop-parse-args parse) t)
    (multiple-value-bind
	(result-costs result-scs)
	(compute-costs-and-restrictions-list (vop-parse-results parse) nil)
      `(
	:cost ,(vop-parse-cost parse)
	
	:arg-costs ',arg-costs
	:arg-load-scs ',arg-scs
	:result-costs ',result-costs
	:result-load-scs ',result-scs
	
	:more-arg-costs
	',(if (vop-parse-more-args parse)
	      (compute-loading-costs-if-any (vop-parse-more-args parse) t)
	      nil)
	
	:more-result-costs
	',(if (vop-parse-more-results parse)
	      (compute-loading-costs-if-any (vop-parse-more-results parse) nil)
	      nil)))))


;;;; Operand checking and stuff:

;;; PARSE-OPERAND-TYPES  --  Internal
;;;
;;;    Given a list of arg/result restrictions, check for valid syntax and
;;; convert to canonical form.
;;;
(defun parse-operand-types (specs args-p)
  (declare (list specs))
  (labels ((parse-operand-type (spec)
	     (cond ((eq spec '*) spec)
		   ((symbolp spec)
		    (let ((alias (gethash spec
					  (backend-primitive-type-aliases
					   *target-backend*))))
		      (if alias
			  (parse-operand-type alias)
			  `(:or ,spec))))
		   ((atom spec)
		    (error "Bad thing to be a operand type: ~S." spec))
		   (t
		    (case (first spec)
		      (:or
		       (collect ((results))
			 (results :or)
			 (dolist (item (cdr spec))
			   (unless (symbolp item)
			     (error "Bad PRIMITIVE-TYPE name in ~S: ~S"
				    spec item))
			   (let ((alias
				  (gethash item
					   (backend-primitive-type-aliases
					    *target-backend*))))
			     (if alias
				 (let ((alias (parse-operand-type alias)))
				   (unless (eq (car alias) :or)
				     (error "Can't include primitive-type ~
				             alias ~S in a :OR restriction: ~S."
					    item spec))
				   (dolist (x (cdr alias))
				     (results x)))
				 (results item))))
			 (remove-duplicates (results)
					    :test #'eq
					    :start 1)))
		      (:constant
		       (unless args-p
			 (error "Can't :CONSTANT for a result."))
		       (unless (= (length spec) 2)
			 (error "Bad :CONSTANT argument type spec: ~S." spec))
		       spec)
		      (t
		       (error "Bad thing to be a operand type: ~S." spec)))))))
    (mapcar #'parse-operand-type specs)))


;;; CHECK-OPERAND-TYPE-SCS  --  Internal
;;;
;;;    Check the consistency of Op's Sc restrictions with the specified
;;; primitive-type restriction.  :CONSTANT operands have already been filtered
;;; out, so only :OR and * restrictions are left.
;;;
;;;    We check that every representation allowed by the type can be directly
;;; loaded into some SC in the restriction, and that the type allows every SC
;;; in the restriction.  With *, we require that T satisfy the first test, and
;;; omit the second.
;;;
(defun check-operand-type-scs (parse op type load-p)
  (declare (type vop-parse parse) (type operand-parse op))
  (let ((ptypes (if (eq type '*) (list 't) (rest type)))
	(scs (operand-parse-scs op)))
    (when scs
      (multiple-value-bind (costs load-scs)
			   (compute-loading-costs op load-p)
	(declare (ignore costs))
	(dolist (ptype ptypes)
	  (unless (dolist (rep (primitive-type-scs
				(meta-primitive-type-or-lose ptype))
			       nil)
		    (when (svref load-scs rep) (return t)))
	    (error "In the ~A ~:[result~;argument~] to VOP ~S,~@
	            none of the SCs allowed by the operand type ~S can ~
		    directly be loaded~@
		    into any of the restriction's SCs:~%  ~S~:[~;~@
		    [* type operand must allow T's SCs.]~]"
		   (operand-parse-name op) load-p (vop-parse-name parse)
		   ptype
		   scs (eq type '*)))))
	  
      (dolist (sc scs)
	(unless (or (eq type '*)
		    (dolist (ptype ptypes nil)
		      (when (meta-sc-allowed-by-primitive-type
			     (meta-sc-or-lose sc)
			     (meta-primitive-type-or-lose ptype))
			(return t))))
	  (warn "~:[Result~;Argument~] ~A to VOP ~S~@
	         has SC restriction ~S which is ~
		 not allowed by the operand type:~%  ~S"
		load-p (operand-parse-name op) (vop-parse-name parse)
		sc type)))))

  (undefined-value))

;;; Check-Operand-Types  --  Internal
;;;
;;;    If the operand types are specified, then check the number specified
;;; against the number of defined operands.
;;;
(defun check-operand-types (parse ops more-op types load-p)
  (declare (type vop-parse parse) (list ops)
	   (type (or list (member :unspecified)) types)
	   (type (or operand-parse null) more-op))
  (unless (eq types :unspecified)
    (let ((num (+ (length ops) (if more-op 1 0))))
      (unless (= (count-if-not #'(lambda (x)
				   (and (consp x)
					(eq (car x) :constant)))
			       types)
		 num)
	(error "Expected ~D ~:[result~;argument~] type~P: ~S."
	       num load-p types num)))
    
    (when more-op
      (let ((mtype (car (last types))))
	(when (and (consp mtype) (eq (first mtype) :constant))
	  (error "Can't use :CONSTANT on VOP more args.")))))
  
  (when (vop-parse-translate parse)
    (let ((types (specify-operand-types types ops more-op)))
      (mapc #'(lambda (x y)
		(check-operand-type-scs parse x y load-p))
	    (if more-op (butlast ops) ops)
	    (remove-if #'(lambda (x)
			   (and (consp x)
				(eq (car x) ':constant)))
		       (if more-op (butlast types) types)))))
  
  (undefined-value))

;;; Grovel-Operands  --  Internal
;;;
;;;    Compute stuff that can only be computed after we are done parsing
;;; everying.  We set the VOP-Parse-Operands, and do various error checks.
;;;
(defun grovel-operands (parse)
  (declare (type vop-parse parse))

  (setf (vop-parse-operands parse)
	(append (vop-parse-args parse)
		(if (vop-parse-more-args parse)
		    (list (vop-parse-more-args parse)))
		(vop-parse-results parse)
		(if (vop-parse-more-results parse)
		    (list (vop-parse-more-results parse)))
		(vop-parse-temps parse)))

  (check-operand-types parse
		       (vop-parse-args parse)
		       (vop-parse-more-args parse)
		       (vop-parse-arg-types parse)
		       t)

  
  (check-operand-types parse
		       (vop-parse-results parse)
		       (vop-parse-more-results parse)
		       (vop-parse-result-types parse)
		       nil)

  (undefined-value))


;;;; Function translation stuff.

;;; Set-Up-Function-Translation  --  Internal
;;;
;;;    Return forms to establish this VOP as a IR2 translation template for the
;;; :Translate functions specified in the VOP-Parse.  We also set the
;;; Predicate attribute for each translated function when the VOP is
;;; conditional, causing IR1 conversion to ensure that a call to the translated
;;; is always used in a predicate position.
;;;
(defun set-up-function-translation (parse n-template)
  (declare (type vop-parse parse))
  (mapcar #'(lambda (name)
	      `(let ((info (function-info-or-lose ',name)))
		 (setf (function-info-templates info)
		       (adjoin-template ,n-template
					(function-info-templates info)))
		 ,@(when (vop-parse-conditional-p parse)
		     '((setf (function-info-attributes info)
			     (attributes-union
			      (ir1-attributes predicate)
			      (function-info-attributes info)))))))
	  (vop-parse-translate parse)))


;;; Make-Operand-Type  --  Internal
;;;
;;;    Return a form that can be evaluated to get the TEMPLATE operand type
;;; restriction from the given specification.
;;;
(defun make-operand-type (type)
  (cond ((eq type '*) ''*)
	((symbolp type)
	 ``(:or ,(primitive-type-or-lose ',type)))
	(t
	 (ecase (first type)
	   (:or
	    ``(:or ,,@(mapcar #'(lambda (type)
				   `(primitive-type-or-lose ',type))
			       (rest type))))
	   (:constant
	    ``(:constant ,#'(lambda (x)
			      (typep x ',(second type)))
			 ,',(second type)))))))


;;; Specify-Operand-Types  --  Internal
;;;
(defun specify-operand-types (types ops more-ops)
  (if (eq types :unspecified)
      (make-list (+ (length ops) (if more-ops 1 0)) :initial-element '*)
      types))

;;; Make-VOP-Info-Types  --  Internal
;;;
;;;    Return a list of forms to use as keyword args to Make-VOP-Info for
;;; setting up the template argument and result types.  Here we make an initial
;;; dummy Template-Type, since it is awkward to compute the type until the
;;; template has been made.
;;;
(defun make-vop-info-types (parse)
  (let* ((more-args (vop-parse-more-args parse))
	 (all-args (specify-operand-types (vop-parse-arg-types parse)
					  (vop-parse-args parse)
					  more-args))
	 (args (if more-args (butlast all-args) all-args))
	 (more-arg (when more-args (car (last all-args))))
	 (more-results (vop-parse-more-results parse))
	 (all-results (specify-operand-types (vop-parse-result-types parse)
					     (vop-parse-results parse)
					     more-results))
	 (results (if more-results (butlast all-results) all-results))
	 (more-result (when more-results (car (last all-results))))
	 (conditional (vop-parse-conditional-p parse)))
    
    `(
      :type (specifier-type '(function () nil))
      :arg-types (list ,@(mapcar #'make-operand-type args))
      :more-args-type ,(when more-args (make-operand-type more-arg))
      :result-types ,(if conditional
			 :conditional
			 `(list ,@(mapcar #'make-operand-type results)))
      :more-results-type ,(when more-results
			    (make-operand-type more-result)))))


;;;; Set up VOP-Info:

(defconstant slot-inherit-alist
  '((:generator-function . vop-info-generator-function)))

;;; Inherit-VOP-Info  --  Internal
;;;
;;;    Something to help with inheriting VOP-Info slots.  We return a
;;; keyword/value pair that can be passed to the constructor.  Slot is the
;;; keyword name of the slot, Parse is a form that evaluates to the VOP-Parse
;;; structure for the VOP inherited.  If Parse is NIL, then we do nothing.  If
;;; the Test form evaluates to true, then we return a form that selects the
;;; named slot from the VOP-Info structure corresponding to Parse.  Otherwise,
;;; we return the Form so that the slot is recomputed.
;;;
(defmacro inherit-vop-info (slot parse test form)
  `(if (and ,parse ,test)
       (list ,slot `(,',(or (cdr (assoc slot slot-inherit-alist))
			    (error "Unknown slot ~S." slot))
		     (template-or-lose ',(vop-parse-name ,parse))))
       (list ,slot ,form)))

;;; Set-Up-VOP-Info  --  Internal
;;;
;;;    Return a form that creates a VOP-Info structure which describes VOP.
;;;
(defun set-up-vop-info (iparse parse)
  (declare (type vop-parse parse) (type (or vop-parse null) iparse))
  (let ((same-operands
	 (and iparse
	      (equal (vop-parse-operands parse)
		     (vop-parse-operands iparse))
	      (equal (vop-parse-info-args iparse)
		     (vop-parse-info-args parse))))
	(variant (vop-parse-variant parse)))

    (let ((nvars (length (vop-parse-variant-vars parse))))
      (unless (= (length variant) nvars)
	(error "Expected ~D variant values: ~S." nvars variant)))

    `(make-vop-info
      :name ',(vop-parse-name parse)
      ,@(make-vop-info-types parse)
      :guard ,(when (vop-parse-guard parse)
		`#'(lambda () ,(vop-parse-guard parse)))
      :note ',(vop-parse-note parse)
      :info-arg-count ,(length (vop-parse-info-args parse))
      :policy ',(vop-parse-policy parse)
      :save-p ',(vop-parse-save-p parse)
      :move-args ',(vop-parse-move-args parse)
      :effects (vop-attributes ,@(vop-parse-effects parse))
      :affected (vop-attributes ,@(vop-parse-affected parse))
      ,@(make-costs-and-restrictions parse)
      ,@(make-emit-function-and-friends parse)
      ,@(inherit-vop-info :generator-function iparse
	  (and same-operands
	       (equal (vop-parse-body parse) (vop-parse-body iparse)))
	  (unless (eq (vop-parse-body parse) :unspecified)
	    (make-generator-function parse)))
      :variant (list ,@variant))))



;;; Define-VOP  --  Public
;;;
;;;    Parse the syntax into a VOP-Parse structure, and then expand into code
;;; that creates the appropriate VOP-Info structure at load time.  We implement
;;; inheritance by copying the VOP-Parse structure for the inherited structure.
;;;
(defmacro define-vop ((name &optional inherits) &rest specs)
  "Define-VOP (Name [Inherits]) Spec*
  Define the symbol Name to be a Virtual OPeration in the compiler.  If
  specified, Inherits is the name of a VOP that we default unspecified
  information from.  Each Spec is a list beginning with a keyword indicating
  the interpretation of the other forms in the Spec:
  
  :Args {(Name {Key Value}*)}*
  :Results {(Name {Key Value}*)}*
      The Args and Results are specifications of the operand TNs passed to the
      VOP.  If there is an inherited VOP, any unspecified options are defaulted
      from the inherited argument (or result) of the same name.  The following
      operand options are defined: 

      :SCs (SC*)
	  :SCs specifies good SCs for this operand.  Other SCs will be
	  penalized according to move costs.  A load TN will be allocated if
	  necessary, guaranteeing that the operand is always one of the
	  specified SCs.

      :Load-TN Load-Name
          Load-Name is bound to the load TN allocated for this operand, or to
	  NIL if no load TN was allocated.

      :Load-If Expression
          Controls whether automatic operand loading is done.  Expression is
	  evaluated with the fixed operand TNs bound.  If Expression is true,
	  then loading is done and the variable is bound to the load TN in
	  the generator body.  Otherwise, loading is not done, and the variable
	  is bound to the actual operand.

      :More T-or-NIL
	  If specified, Name is bound to the TN-Ref for the first argument or
	  result following the fixed arguments or results.  A more operand must
	  appear last, and cannot be targeted or restricted.

      :Target Operand
	  This operand is targeted to the named operand, indicating a desire to
	  pack in the same location.  Not legal for results.

      :From Time-Spec
      :To Time-Spec
	  Specify the beginning or end of the operand's lifetime.  :From can
	  only be used with results, and :To only with arguments.  The default
	  for the N'th argument/result is (:ARGUMENT N)/(:RESULT N).  These
	  options are necessary primarily when operands are read or written out
	  of order.
   
  :Conditional
      This is used in place of :RESULTS with conditional branch VOPs.  There
      are no result values: the result is a transfer of control.  The target
      label is passed as the first :INFO arg.  The second :INFO arg is true if
      the sense of the test should be negated.  A side-effect is to set the
      PREDICATE attribute for functions in the :TRANSLATE option.
  
  :Temporary ({Key Value}*) Name*
      Allocate a temporary TN for each Name, binding that variable to the TN
      within the body of the generators.  In addition to :Target (which is 
      is the same as for operands), the following options are
      defined:

      :SC SC-Name
      :Offset SB-Offset
	  Force the temporary to be allocated in the specified SC with the
	  specified offset.  Offset is evaluated at macroexpand time.  If
	  Offset is emitted, the register allocator chooses a free location in
	  SC.  If both SC and Offset are omitted, then the temporary is packed
	  according to its primitive type.

      :From Time-Spec
      :To Time-Spec
	  Similar to the argument/result option, this specifies the start and
	  end of the temporarys' lives.  The defaults are :Load and :Save, i.e.
	  the duration of the VOP.  The other intervening phases are :Argument,
	  :Eval and :Result.  Non-zero sub-phases can be specified by a list,
	  e.g. by default the second argument's life ends at (:Argument 1).
 
  :Generator Cost Form*
      Specifies the translation into assembly code. Cost is the estimated cost
      of the code emitted by this generator. The body is arbitrary Lisp code
      that emits the assembly language translation of the VOP.  An Assemble
      form is wrapped around the body, so code may be emitted by using the
      local Inst macro.  During the evaluation of the body, the names of the
      operands and temporaries are bound to the actual TNs.
  
  :Effects Effect*
  :Affected Effect*
      Specifies the side effects that this VOP has and the side effects that
      effect its execution.  If unspecified, these default to the worst case.
  
  :Info Name*
      Define some magic arguments that are passed directly to the code
      generator.  The corresponding trailing arguments to VOP or %Primitive are
      stored in the VOP structure.  Within the body of the generators, the
      named variables are bound to these values.  Except in the case of
      :Conditional VOPs, :Info arguments cannot be specified for VOPS that are
      the direct translation for a function (specified by :Translate).

  :Ignore Name*
      Causes the named variables to be declared IGNORE in the generator body.

  :Variant Thing*
  :Variant-Vars Name*
      These options provide a way to parameterize families of VOPs that differ
      only trivially.  :Variant makes the specified evaluated Things be the
      \"variant\" associated with this VOP.  :Variant-Vars causes the named
      variables to be bound to the corresponding Things within the body of the
      generator.

  :Variant-Cost Cost
      Specifies the cost of this VOP, overriding the cost of any inherited
      generator.

  :Note {String | NIL}
      A short noun-like phrase describing what this VOP \"does\", i.e. the
      implementation strategy.  If supplied, efficency notes will be generated
      when type uncertainty prevents :TRANSLATE from working.  NIL inhibits any
      efficency note.

  :Arg-Types    {* | PType | (:OR PType*) | (:CONSTANT Type)}*
  :Result-Types {* | PType | (:OR PType*)}*
      Specify the template type restrictions used for automatic translation.
      If there is a :More operand, the last type is the more type.  :CONSTANT
      specifies that the argument must be a compile-time constant of the
      specified Lisp type.  The constant values of :CONSTANT arguments are
      passed as additional :INFO arguments rather than as :ARGS.
  
  :Translate Name*
      This option causes the VOP template to be entered as an IR2 translation
      for the named functions.

  :Policy {:Small | :Fast | :Safe | :Fast-Safe}
      Specifies the policy under which this VOP is the best translation.

  :Guard Form
      Specifies a Form that is evaluated in the global environment.  If
      form returns NIL, then emission of this VOP is prohibited even when
      all other restrictions are met.

  :VOP-Var Name
  :Node-Var Name
      In the generator, bind the specified variable to the VOP or the Node that
      generated this VOP.

  :Save-P {NIL | T | :Compute-Only | :Force-To-Stack}
      Indicates how a VOP wants live registers saved.

  :Move-Args {NIL | :Full-Call | :Local-Call | :Known-Return}
      Indicates if and how the more args should be moved into a different
      frame."
  (check-type name symbol)
  
  (let* ((iparse (when inherits
		   (vop-parse-or-lose inherits)))
	 (parse (if inherits
		    (copy-vop-parse iparse)
		    (make-vop-parse)))
	 (n-res (gensym)))
    (setf (vop-parse-name parse) name)
    (setf (vop-parse-inherits parse) inherits)

    (parse-define-vop parse specs)
    (grovel-operands parse)
      
    `(progn
       (eval-when (compile load eval)
	 (setf (gethash ',name (backend-parsed-vops *target-backend*))
	       ',parse))

       (let ((,n-res ,(set-up-vop-info iparse parse)))
	 (setf (gethash ',name (backend-template-names *target-backend*))
	       ,n-res)
	 (setf (template-type ,n-res)
	       (specifier-type (template-type-specifier ,n-res)))
	 ,@(set-up-function-translation parse n-res))
       ',name)))


;;;; Emission macros:

;;; Make-Operand-List  --  Internal
;;;
;;;    Return code to make a list of VOP arguments or results, linked by
;;; TN-Ref-Across.  The first value is code, the second value is LET* forms,
;;; and the third value is a variable that evaluates to the head of the list,
;;; or NIL if there are no operands.  Fixed is a list of forms that evaluate to
;;; TNs for the fixed operands.  TN-Refs will be made for these operands
;;; according using the specified value of Write-P.  More is an expression that
;;; evaluates to a list of TN-Refs that will be made the tail of the list.  If
;;; it is constant NIL, then we don't bother to set the tail.
;;;
(defun make-operand-list (fixed more write-p)
  (collect ((forms)
	    (binds))
    (let ((n-head nil)
	  (n-prev nil))
      (dolist (op fixed)
	(let ((n-ref (gensym)))
	  (binds `(,n-ref (reference-tn ,op ,write-p)))
	  (if n-prev
	      (forms `(setf (tn-ref-across ,n-prev) ,n-ref))
	    (setq n-head n-ref))
	  (setq n-prev n-ref)))

      (when more
	(let ((n-more (gensym)))
	  (binds `(,n-more ,more))
	  (if n-prev
	      (forms `(setf (tn-ref-across ,n-prev) ,n-more))
	      (setq n-head n-more))))

      (values (forms) (binds) n-head))))


;;; Emit-Template  -- Interface
;;;
(defmacro emit-template (node block template args results &optional info)
  "Emit-Template Node Block Template Args Results [Info]
  Call the emit function for Template, linking the result in at the end of
  Block."
  (let ((n-first (gensym))
	(n-last (gensym)))
    (once-only ((n-node node)
		(n-block block)
		(n-template template))
      `(multiple-value-bind
	   (,n-first ,n-last)
	   (funcall (template-emit-function ,n-template)
		    ,n-node ,n-block ,n-template ,args ,results
		    ,@(when info `(,info)))
	 (insert-vop-sequence ,n-first ,n-last ,n-block nil)))))

  
;;; VOP  --  Interface
;;;
(defmacro vop (name node block &rest operands)
  "VOP Name Node Block Arg* Info* Result*
  Emit the VOP (or other template) Name at the end of the IR2-Block Block,
  using Node for the source context.  The interpretation of the remaining
  arguments depends on the number of operands of various kinds that are
  declared in the template definition.  VOP cannot be used for templates that
  have more-args or more-results, since the number of arguments and results is
  indeterminate for these templates.  Use VOP* instead.
  
  Args and Results are the TNs that are to be referenced by the template
  as arguments and results.  If the template has codegen-info arguments, then
  the appropriate number of Info forms following the Arguments are used for
  codegen info."
  (let* ((parse (vop-parse-or-lose name))
	 (arg-count (length (vop-parse-args parse)))
	 (result-count (length (vop-parse-results parse)))
	 (info-count (length (vop-parse-info-args parse)))
	 (noperands (+ arg-count result-count info-count))
	 (n-node (gensym))
	 (n-block (gensym))
	 (n-template (gensym)))
    
    (when (or (vop-parse-more-args parse) (vop-parse-more-results parse))
      (error "Cannot use VOP with variable operand count templates."))
    (unless (= noperands (length operands))
      (error "Called with ~D operands, but was expecting ~D."
	     (length operands) noperands))
    
    (multiple-value-bind
	(acode abinds n-args)
	(make-operand-list (subseq operands 0 arg-count) nil nil)
      (multiple-value-bind
	  (rcode rbinds n-results)
	  (make-operand-list (subseq operands (+ arg-count info-count)) nil t)
	
	(collect ((ibinds)
		  (ivars))
	  (dolist (info (subseq operands arg-count (+ arg-count info-count)))
	    (let ((temp (gensym)))
	      (ibinds `(,temp ,info))
	      (ivars temp)))
	  
	  `(let* ((,n-node ,node)
		  (,n-block ,block)
		  (,n-template (template-or-lose ',name *backend*))
		  ,@abinds
		  ,@(ibinds)
		  ,@rbinds)
	     ,@acode
	     ,@rcode
	     (emit-template ,n-node ,n-block ,n-template ,n-args
			    ,n-results 
			    ,@(when (ivars)
				`((list ,@(ivars)))))
	     (undefined-value)))))))


;;; VOP*  --  Interface
;;;
(defmacro vop* (name node block args results &rest info)
  "VOP* Name Node Block (Arg* More-Args) (Result* More-Results) Info*
  Like VOP, but allows for emission of templates with arbitrary numbers of
  arguments, and for emission of templates using already-created TN-Ref lists.

  The Arguments and Results are TNs to be referenced as the first arguments
  and results to the template.  More-Args and More-Results are heads of TN-Ref
  lists that are added onto the end of the TN-Refs for the explicitly supplied
  operand TNs.  The TN-Refs for the more operands must have the TN and Write-P
  slots correctly initialized.

  As with VOP, the Info forms are evaluated and passed as codegen info
  arguments."
  (check-type args cons)
  (check-type results cons)
  (let* ((parse (vop-parse-or-lose name))
	 (arg-count (length (vop-parse-args parse)))
	 (result-count (length (vop-parse-results parse)))
	 (info-count (length (vop-parse-info-args parse)))
	 (fixed-args (butlast args))
	 (fixed-results (butlast results))
	 (n-node (gensym))
	 (n-block (gensym))
	 (n-template (gensym)))
    
    (unless (or (vop-parse-more-args parse)
		(<= (length fixed-args) arg-count))
      (error "Too many fixed arguments."))
    (unless (or (vop-parse-more-results parse)
		(<= (length fixed-results) result-count))
      (error "Too many fixed results."))
    (unless (= (length info) info-count)
      (error "Expected ~D info args." info-count))
    
    (multiple-value-bind
	(acode abinds n-args)
	(make-operand-list fixed-args (car (last args)) nil)
      (multiple-value-bind
	  (rcode rbinds n-results)
	  (make-operand-list fixed-results (car (last results)) t)
	
	`(let* ((,n-node ,node)
		(,n-block ,block)
		(,n-template (template-or-lose ',name *backend*))
		,@abinds
		,@rbinds)
	   ,@acode
	   ,@rcode
	   (emit-template ,n-node ,n-block ,n-template ,n-args ,n-results
			  ,@(when info
			      `((list ,@info))))
	   (undefined-value))))))


;;;; Random macros:

;;; SC-Case  --  Public
;;;
(defmacro sc-case (tn &rest forms)
  "SC-Case TN {({(SC-Name*) | SC-Name | T} Form*)}*
  Case off of TN's SC.  The first clause containing TN's SC is evaulated,
  returning the values of the last form.  A clause beginning with T specifies a
  default.  If it appears, it must be last.  If no default is specified, and no
  clause matches, then an error is signalled."
  (let ((n-sc (gensym))
	(n-tn (gensym)))
    (collect ((clauses))
      (do ((cases forms (rest cases)))
	  ((null cases)
	   (clauses `(t (error "Unknown SC to SC-Case for ~S:~%  ~S" ,n-tn
			       (sc-name (tn-sc ,n-tn))))))
	(let ((case (first cases)))
	  (when (atom case) 
	    (error "Illegal SC-Case clause: ~S." case))
	  (let ((head (first case)))
	    (when (eq head t)
	      (when (rest cases)
		(error "T case is not last in SC-Case."))
	      (clauses `(t nil ,@(rest case)))
	      (return))
	    (clauses `((or ,@(mapcar #'(lambda (x)
					 `(eql ,(meta-sc-number-or-lose x)
					       ,n-sc))
				     (if (atom head) (list head) head)))
		       nil ,@(rest case))))))

      `(let* ((,n-tn ,tn)
	      (,n-sc (sc-number (tn-sc ,n-tn))))
	 (cond ,@(clauses))))))


;;; SC-Is  --  Interface
;;;
(defmacro sc-is (tn &rest scs)
  "SC-Is TN SC*
  Returns true if TNs SC is any of the named SCs, false otherwise."
  (once-only ((n-sc `(sc-number (tn-sc ,tn))))
    `(or ,@(mapcar #'(lambda (x)
		       `(eql ,n-sc ,(meta-sc-number-or-lose x)))
		   scs))))

;;; Do-IR2-Blocks  --  Interface
;;;
(defmacro do-ir2-blocks ((block-var component &optional result)
			 &body forms)
  "Do-IR2-Blocks (Block-Var Component [Result]) Form*
  Iterate over the IR2 blocks in component, in emission order."
  `(do ((,block-var (block-info (component-head ,component))
		    (ir2-block-next ,block-var)))
       ((null ,block-var) ,result)
     ,@forms))


;;; DO-LIVE-TNS  --  Interface
;;;
(defmacro do-live-tns ((tn-var live block &optional result) &body body)
  "DO-LIVE-TNS (TN-Var Live Block [Result]) Form*
  Iterate over all the TNs live at some point, with the live set represented by
  a local conflicts bit-vector and the IR2-Block containing the location."
  (let ((n-conf (gensym))
	(n-bod (gensym))
	(i (gensym))
	(ltns (gensym)))
    (once-only ((n-live live)
		(n-block block))
      `(block nil
	 (flet ((,n-bod (,tn-var) ,@body))
	   ;;
	   ;; Do component-live TNs.
	   (dolist (,tn-var (ir2-component-component-tns
			     (component-info
			      (block-component
			       (ir2-block-block ,n-block)))))
	     (,n-bod ,tn-var))
	   
	   (let ((,ltns (ir2-block-local-tns ,n-block)))
	     ;;
	     ;; Do TNs always-live in this block and live :More TNs.
	     (do ((,n-conf (ir2-block-global-tns ,n-block)
			   (global-conflicts-next ,n-conf)))
		 ((null ,n-conf))
	       (when (or (eq (global-conflicts-kind ,n-conf) :live)
			 (let ((,i (global-conflicts-number ,n-conf)))
			   (and (eq (svref ,ltns ,i) :more)
				(not (zerop (sbit ,n-live ,i))))))
		 (,n-bod (global-conflicts-tn ,n-conf))))
	     ;;
	     ;; Do TNs locally live in the designated live set.
	     (dotimes (,i (ir2-block-local-tn-count ,n-block) ,result)
	       (unless (zerop (sbit ,n-live ,i))
		 (let ((,tn-var (svref ,ltns ,i)))
		   (when (and ,tn-var (not (eq ,tn-var :more)))
		     (,n-bod ,tn-var)))))))))))


;;; DO-ENVIRONMENT-IR2-BLOCKS  --  Interface
;;;
(defmacro do-environment-ir2-blocks ((block-var env &optional result)
				     &body body)
  "DO-ENVIRONMENT-IR2-BLOCKS (Block-Var Env [Result]) Form*
  Iterate over all the IR2 blocks in the environment Env, in emit order."
  (once-only ((n-env env))
    (once-only ((n-first `(node-block
			   (lambda-bind
			    (environment-function ,n-env)))))
      (once-only ((n-tail `(block-info
			    (component-tail
			     (block-component ,n-first)))))
	`(do ((,block-var (block-info ,n-first)
			  (ir2-block-next ,block-var)))
	     ((or (eq ,block-var ,n-tail)
		  (not (eq (ir2-block-environment ,block-var) ,n-env)))
	      ,result)
	   ,@body)))))


