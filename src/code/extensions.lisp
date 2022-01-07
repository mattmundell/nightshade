;;; Extensions to the language.

(in-package "EXTENSIONS")

(export '(letf* letf dovector deletef indenting-further file-comment
	  read-char-no-edit ; FIX Where is this?
	  listen-skip-whitespace concat-pnames
	  iterate once-only collect do-anonymous undefined-value
	  required-argument define-hash-cache defun-cached
	  cache-hash-eq do-hash))

(import 'lisp::whitespace-char-p)


;;; Undefined-Value  --  Public
;;;
;;; FIX This is here until we figure out what to do with it.
;;;
(proclaim '(inline undefined-value))
(defun undefined-value ()
  '%undefined%)

;;; REQUIRED-ARGUMENT  --  Public
;;;
(proclaim '(ftype (function () nil) required-argument))
(defun required-argument ()
  "Signal a \"keyword required\" error.

   This function can be used as the fallback value for keyword arguments
   that must always be supplied.  This function is also useful for
   `defstruct' slot fallback corresponding to required arguments.

   \xlref{empty-type}."
  (error "A keyword argument is required."))

;;; FILE-COMMENT  --  Public
;;;
(defmacro file-comment (string)
  "file-comment $string

   When `compile-file' sees this form at top-level, it places the constant
   string in the run-time source location information.  `describe' will
   print the file comment for the file that a function was defined in.  The
   string is also textually present in the FASL, so the RCS \"ident\"
   command can find it, etc."
  (declare (ignore string))
  '(undefined-value))

(defun skip-whitespace (&optional (stream *standard-input*))
  (loop (let ((char (read-char stream)))
	  (if (not (lisp::whitespacep char))
	      (return (unread-char char stream))))))

(defun listen-skip-whitespace (&optional (stream *standard-input*))
  "See listen.  Any whitespace in the input stream will be flushed."
  (do ((char (read-char-no-hang stream nil nil nil)
	     (read-char-no-hang stream nil nil nil)))
      ((null char) nil)
    (cond ((not (whitespace-char-p char))
	   (unread-char char stream)
	   (return t)))))

;;; These macros waste time as opposed to space.

(defmacro letf* (bindings &body body &environment env)
  "Does what one might expect, saving the old values and setting the generalized
   variables to the new values in sequence.  Unwind-protects and get-setf-method
   are used to preserve the semantics one might expect in analogy to let*,
   and the once-only evaluation of subforms."
  (labels ((do-bindings
	    (bindings)
	    (cond ((null bindings) body)
		  (t (multiple-value-bind (dummies vals newval setter getter)
					  (get-setf-method (caar bindings) env)
		       (let ((save (gensym)))
			 `((let* (,@(mapcar #'list dummies vals)
				  (,(car newval) ,(cadar bindings))
				  (,save ,getter))
			     (unwind-protect
			       (progn ,setter
				      ,@(do-bindings (cdr bindings)))
			       (setq ,(car newval) ,save)
			       ,setter)))))))))
    (car (do-bindings bindings))))

(defmacro letf (bindings &body body &environment env)
  "Like letf*, but evaluates all the implicit subforms and new values of all
   the implied setfs before altering any values.  However, the store forms
   (see get-setf-method) must still be evaluated in sequence.  Uses unwind-
   protects to protect the environment."
  (let (temps)
    (labels
      ((do-bindings
	(bindings)
	(cond ((null bindings) body)
	      (t (let ((binding (car bindings)))
		   (multiple-value-bind (dummies vals newval setter getter)
					(get-setf-method (car binding) env)
		     (let ((save (gensym)))
		       (mapcar #'(lambda (a b) (push (list a b) temps))
			       dummies vals)
		       (push (list save getter) temps)
		       (push (list (car newval) (cadr binding)) temps)
		       `((unwind-protect
			   (progn ,setter
				  ,@(do-bindings (cdr bindings)))
			   (setq ,(car newval) ,save)
			   ,setter)))))))))
      (let ((form (car (do-bindings bindings))))
	`(let* ,(nreverse temps)
	   ,form)))))

(define-setf-method logbitp (index int &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
		       (get-setf-method int env)
    (let ((ind (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,ind ,@temps)
	      `(,index
		,@vals)
	      (list store)
	      `(let ((,stemp
		      (dpb (if ,store 1 0) (byte 1 ,ind) ,access-form)))
		 ,store-form
		 ,store)
	      `(logbitp ,ind ,access-form)))))

;;; Indenting-Further is a user-level macro which may be used to locally increment
;;; the indentation of a stream.

(defmacro indenting-further (stream more &rest body)
  "Causes the output of the indenting Stream to indent More spaces.  More is
   evaluated twice."
  `(unwind-protect
     (progn
      (incf (lisp::indenting-stream-indentation ,stream) ,more)
      ,@body)
     (decf (lisp::indenting-stream-indentation ,stream) ,more)))

;;; Deletef

(defmacro deletef (elt list &rest keys &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method list env)
    (let ((eltsym (gensym))
	  (listsym (gensym)))
      `(let* ((,eltsym ,elt)
	      ,@(mapcar #'list dummies vals)
	      (,listsym ,getter)
	      (,(car newval) (delete ,eltsym ,listsym ,@keys)))
	 ,setter))))

(defmacro dovector ((elt vector) &rest forms)
  "Just like dolist, but with one-dimensional arrays."
  (let ((index (gensym))
	(length (gensym))
	(vec (gensym)))
    `(let ((,vec ,vector))
       (do ((,index 0 (1+ ,index))
	    (,length (length ,vec)))
	   ((>= ,index ,length) nil)
	 (let ((,elt (aref ,vec ,index)))
	   ,@forms)))))

(eval-when (compile load eval)
  (defun concat-pnames (name1 name2)
    (declare (symbol name1 name2))
    (if name1
	(intern (concatenate 'simple-string (symbol-name name1)
			     (symbol-name name2)))
	name2)))

;;; Iterate  --  Public
;;;
(defmacro iterate (name binds &body body)
  "iterate name ({(var initial-value)}*) declaration* form*

   Iterate using `labels'.  Create a local function $name with the
   specified \"var\"s as arguments and the \"declarations\" and \"forms\"
   as body.  Then call the function with the \"initial-values\", returning
   the result."
  (dolist (x binds)
    (or (and (listp x)
	     (= (length x) 2))
	(error "Malformed iterate variable spec: ~S." x)))

  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))


;;;; The Collect macro.

;;; Collect-Normal-Expander  --  Internal
;;;
;;; This function does the real work of macroexpansion for normal
;;; collection macros.  N-Value is the name of the variable which holds the
;;; current value.  Fun is the function which does collection.  Forms is
;;; the list of forms whose values we are supposed to collect.
;;;
(defun collect-normal-expander (n-value fun forms)
  `(progn
    ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
    ,n-value))

;;; Collect-List-Expander  --  Internal
;;;
;;; This function deals with the list collection case.  N-Tail is the
;;; pointer to the current tail of the list, which is NIL if the list is
;;; empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
      ,@(mapcar #'(lambda (form)
		    `(let ((,n-res (cons ,form nil)))
		       (cond (,n-tail
			      (setf (cdr ,n-tail) ,n-res)
			      (setq ,n-tail ,n-res))
			     (t
			      (setq ,n-tail ,n-res  ,n-value ,n-res)))))
		forms)
      ,n-value)))

;;; Collect  --  Public
;;;
(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*

   Collect values.  Each Name specifies a collection to which values can be
   added during the evaluation of the body.  The name of the collection is
   used to define a local macro, as with MACROLET.  Within the body, this
   macro will evaluate each of its arguments and collect the result,
   returning the current value after the collection is done.  The body is
   evaluated as a PROGN; to get the final values when you are done, just
   call the collection macro with no arguments.

   Initial-Value is the value that the collection starts out with, which
   defaults to NIL.  Function is the function which does the collection.
   It is a function which will accept two arguments: the value to be
   collected and the current collection.  The result of the function is
   made the new value for the collection.  As a special-case the Function
   may be Collect, which results in a list built in forward order; this is
   the default.  If an Initial-Value is supplied for Collect, the stuff
   will be rplacd'd onto the end.  Note that Function may be anything that
   can appear in the functional position, including macros and lambdas."

  (let ((macros ())
	(binds ()))
    (dolist (spec collections)
      (or (<= 1 (length spec) 3)
	  (error "Malformed collection specifier: ~S." spec))
      (let ((n-value (gensym))
	    (name (first spec))
	    (default (second spec))
	    (kind (or (third spec) 'collect)))
	(push `(,n-value ,default) binds)
	(if (eq kind 'collect)
	    (let ((n-tail (gensym)))
	      (if default
		  (push `(,n-tail (last ,n-value)) binds)
		  (push n-tail binds))
	      (push `(,name (&rest args)
			    (collect-list-expander ',n-value ',n-tail args))
		    macros))
	    (push `(,name (&rest args)
			  (collect-normal-expander ',n-value ',kind args))
		  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))


;;;; The Once-Only macro.

;;; Once-Only  --  Interface
;;;
;;; Once-Only is a utility useful in writing source transforms and macros.
;;; It provides an easy way to wrap a let around some code to ensure that
;;; some forms are only evaluated once.
;;;
(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*

   Create a Let* which evaluates each Value-Expression, binding a temporary
   variable to the result, and wrapping the Let* around the result of the
   evaluation of Body.  Within the body, each Var is bound to the
   corresponding temporary variable."
  (iterate frob
	   ((specs specs)
	    (body body))
    (if (null specs)
	`(progn ,@body)
	(let ((spec (first specs)))
	  (when (/= (length spec) 2)
	    (error "Malformed Once-Only binding spec: ~S." spec))
	  (let ((name (first spec))
		(exp-temp (gensym)))
	    `(let ((,exp-temp ,(second spec))
		   (,name (gensym "OO-")))
	       `(let ((,,name ,,exp-temp))
		  ,,(frob (rest specs) body))))))))


;;;; do-*.

;;; ### Bootstrap hack...  Renamed to avoid clobbering function in bootstrap
;;; environment.
;;;
(defun lisp::generate-do-loop (varlist endlist code decl bind step name block exit)
  (declare (ignore name))
  (let* ((inits ())
	 (steps ())
	 (l1 (gensym))
	 (l2 (gensym)))
    ;; Check arguments.
    (or (listp varlist)
	(error "$varlist (first argument) must be a list."))
    (or (listp endlist)
	(error "$endlist (second argument) must be a list."))
    ;; Parse the varlist to get inits and steps.
    (dolist (v varlist)
      (cond ((symbolp v) (push v inits))
	    ((listp v)
	     (or (symbolp (first v))
		 (error "Step variable must be a symbol: ~S" (first v)))
	     (case (length v)
	       (1 (push (first v) inits))
	       (2 (push v inits))
	       (3 (push (list (first v) (second v)) inits)
		  (setq steps (list* (third v) (first v) steps)))
	       (t (error "~S is too long for a variable element." v))))
	    (t (error "Variable element ~S must be a symbol or a list." v))))
    ;; And finally construct the new form.
    `(block ,block
       (,bind ,(nreverse inits)
	,@decl
	(tagbody
	 ;; FIX why jump over the code first?
	 (go ,L2)
	 ,L1
	 ,@code
	 (,step ,@(nreverse steps))
  	 ,L2
	 (,exit ,(car endlist) (go ,L1))
	 (return-from ,block (progn ,@(cdr endlist))))))))

(defmacro do-anonymous (varlist endlist &body (body decls))
  "do-anonymous ({(var [init] [step])}*) (test exit-form*) declaration* form*

   Like `do', but has no implicit NIL block.  Each Var is initialized in
   parallel to the value of the specified Init form.  On subsequent
   iterations, the Vars are assigned the value of the Step form (if any) in
   parallel.  The Test is evaluated before each evaluation of the body
   Forms.  When the Test is true, the Exit-Forms are evaluated as a
   `progn', with the result being the value of the `do'."
  (lisp::generate-do-loop varlist endlist body decls 'let 'psetq
		       'do-anonymous (gensym) 'or))

(defmacro do-hash ((key-var value-var table &optional result)
		   &body (body decls))
  "do-hash (key-var value-var table [result]) declaration* form*

   Iterate over the entries in hash-table $table."
  (let ((gen (gensym))
	(n-more (gensym)))
    `(with-hash-table-iterator (,gen ,table)
       (loop
	 (multiple-value-bind (,n-more ,key-var ,value-var)
			      (,gen)
	   ,@decls
	   (unless ,n-more (return ,result))
	   ,@body)))))

;;; Public.
;;;
(defmacro do-strings ((string-var value-var table &optional result) &body forms)
  "do-strings ($string-var $value-var $table [$result]) {declaration}* {form}*

   Iterate over the strings in a string Table.  String-Var and Value-Var
   are bound to the string and value respectively of each successive entry
   in the string-table Table in alphabetical order.  If supplied, Result is
   a form to evaluate to get the return value."
  (let ((value-nodes (gensym))
	(num-nodes (gensym))
	(value-node (gensym))
	(i (gensym)))
    `(let ((,value-nodes (string-table-value-nodes ,table))
	   (,num-nodes (string-table-num-nodes ,table)))
       (dotimes (,i ,num-nodes ,result)
	 (declare (fixnum ,i))
	 (let* ((,value-node (svref ,value-nodes ,i))
		(,value-var (value-node-value ,value-node))
		(,string-var (value-node-proper ,value-node)))
	   (declare (simple-string ,string-var))
	   (declare (ignorable ,value-var ,string-var))
	   ,@forms)))))

;;; Public.
;;;
(defmacro do-lines ((line-var stream) &body forms)
  "do-lines (line-var buffer {mark}) {declaration}* {form}*

   Execute $forms for each line in $stream with the line string bound to
   $line-var."
  `(loop for ,line-var = (read-line ,stream ())
     while ,line-var do
     ,@forms))


;;;; Hash cache utility.

(eval-when (compile load eval)
  (defvar *profile-hash-cache* nil))

;;; DEFINE-HASH-CACHE  --  Public
;;;
;;; :INIT-FORM passed as COLD-LOAD-INIT in type system definitions so that
;;; caches can be created before top-level forms run.
;;;
(defmacro define-hash-cache (name args &key hash-function hash-bits default
				  (init-form 'progn)
				  (values 1))
  "DEFINE-HASH-CACHE Name ({(Arg-Name Test-Function)}*) {Key Value}*

   Define a hash cache that associates some number of argument values to a
   result value.  The Test-Function paired with each Arg-Name is used to compare
   the value for that arg in a cache entry with a supplied arg.  The
   Test-Function must not error when passed NIL as its first arg, but need not
   return any particular value.  Test-Function may be any thing that can be
   place FIX placed? in CAR position.

   Name is used to define functions these functions:

  <name>-CACHE-LOOKUP Arg*
      See if there is an entry for the specified Args in the cache.  The if not
      present, the :DEFAULT keyword (default NIL) determines the result(s).

  <name>-CACHE-ENTER Arg* Value*
      Encache the association of the specified args with Value.

  <name>-CACHE-FLUSH-<arg-name> Arg
      Flush all entries from the cache that have the value Arg for the named
      arg.

  <name>-CACHE-CLEAR
      Reinitialize the cache, invalidating all entries and allowing the
      arguments and result values to be GC'd.

  These other keywords are defined:

  :HASH-BITS <n>
      The size of the cache as a power of 2.

  :HASH-FUNCTION function
      Some thing that can be placed in CAR position which will compute a value
      between 0 and (1- (expt 2 <hash-bits>)).

  :VALUES <n>
      The number of values cached.

   :INIT-FORM <name>
      The DEFVAR for creating the cache is enclosed in a form with the
      specified name.  Default PROGN."

  (let* ((var-name (symbolicate "*" name "-CACHE-VECTOR*"))
	 (nargs (length args))
	 (entry-size (+ nargs values))
	 (size (ash 1 hash-bits))
	 (total-size (* entry-size size))
	 (default-values (if (and (consp default) (eq (car default) 'values))
			     (cdr default)
			     (list default)))
	 (n-index (gensym))
	 (n-cache (gensym)))

    (unless (= (length default-values) values)
      (error "Number of default values ~S differs from :VALUES ~D."
	     default values))

    (collect ((inlines)
	      (forms)
	      (inits)
	      (tests)
	      (sets)
	      (arg-vars)
	      (values-indices)
	      (values-names))
      (dotimes (i values)
	(values-indices `(+ ,n-index ,(+ nargs i)))
	(values-names (gensym)))

      (let ((n 0))
	(dolist (arg args)
	  (unless (= (length arg) 2)
	    (error "Bad arg spec: ~S." arg))
	  (let ((arg-name (first arg))
		(test (second arg)))
	    (arg-vars arg-name)
	    (tests `(,test (svref ,n-cache (+ ,n-index ,n)) ,arg-name))
	    (sets `(setf (svref ,n-cache (+ ,n-index ,n)) ,arg-name))

	    (let ((fun-name (symbolicate name "-CACHE-FLUSH-" arg-name)))
	      (forms
	       `(defun ,fun-name (,arg-name)
		  (do ((,n-index ,(+ (- total-size entry-size) n)
				 (- ,n-index ,entry-size))
		       (,n-cache ,var-name))
		      ((minusp ,n-index))
		    (declare (type fixnum ,n-index))
		    (when (,test (svref ,n-cache ,n-index) ,arg-name)
		      (let ((,n-index (- ,n-index ,n)))
			,@(mapcar #'(lambda (i val)
				      `(setf (svref ,n-cache ,i) ,val))
				  (values-indices)
				  default-values))))
		  (undefined-value)))))
	  (incf n)))

      (when *profile-hash-cache*
	(let ((n-probe (symbolicate "*" name "-CACHE-PROBES*"))
	      (n-miss (symbolicate "*" name "-CACHE-MISSES*")))
	  (inits `(setq ,n-probe 0))
	  (inits `(setq ,n-miss 0))
	  (forms `(defvar ,n-probe))
	  (forms `(defvar ,n-miss))
	  (forms `(declaim (fixnum ,n-miss ,n-probe)))))

      (let ((fun-name (symbolicate name "-CACHE-LOOKUP")))
	(inlines fun-name)
	(forms
	 `(defun ,fun-name ,(arg-vars)
	    ,@(when *profile-hash-cache*
		`((incf ,(symbolicate  "*" name "-CACHE-PROBES*"))))
	    (let ((,n-index (* (,hash-function ,@(arg-vars)) ,entry-size))
		  (,n-cache ,var-name))
	      (declare (type fixnum ,n-index))
	      (cond ((and ,@(tests))
		     (values ,@(mapcar #'(lambda (x) `(svref ,n-cache ,x))
				       (values-indices))))
		    (t
		     ,@(when *profile-hash-cache*
			 `((incf ,(symbolicate  "*" name "-CACHE-MISSES*"))))
		     ,default))))))

      (let ((fun-name (symbolicate name "-CACHE-ENTER")))
	(inlines fun-name)
	(forms
	 `(defun ,fun-name (,@(arg-vars) ,@(values-names))
	    (let ((,n-index (* (,hash-function ,@(arg-vars)) ,entry-size))
		  (,n-cache ,var-name))
	      (declare (type fixnum ,n-index))
	      ,@(sets)
	      ,@(mapcar #'(lambda (i val)
			    `(setf (svref ,n-cache ,i) ,val))
			(values-indices)
			(values-names))
	      (undefined-value)))))

      (let ((fun-name (symbolicate name "-CACHE-CLEAR")))
	(forms
	 `(defun ,fun-name ()
	    (do ((,n-index ,(- total-size entry-size) (- ,n-index ,entry-size))
		 (,n-cache ,var-name))
		((minusp ,n-index))
	      (declare (type fixnum ,n-index))
	      ,@(collect ((arg-sets))
		  (dotimes (i nargs)
		    (arg-sets `(setf (svref ,n-cache (+ ,n-index ,i)) nil)))
		  (arg-sets))
	      ,@(mapcar #'(lambda (i val)
			    `(setf (svref ,n-cache ,i) ,val))
			(values-indices)
			default-values))
	    (undefined-value)))
	(forms `(,fun-name)))

      (inits `(unless (boundp ',var-name)
		(setq ,var-name (make-array ,total-size))))

      `(progn
	 (defvar ,var-name)
	 (,init-form ,@(inits))
	 (proclaim '(type (simple-vector ,total-size) ,var-name))
	 (proclaim '(inline ,@(inlines)))
	 ,@(forms)
	 ',name))))

;;; DEFUN-CACHED  --  Public
;;;
(defmacro defun-cached ((name &rest options &key (values 1) default
			      &allow-other-keys)
			args &body (body decls doc))
  "DEFUN-CACHED (Name {Key Value}*) ({(Arg-Name Test-Function)}*) Form*

   FIX Some syntactic sugar for defining a function whose values are cached
   by DEFINE-HASH-CACHE."
  (let ((default-values (if (and (consp default) (eq (car default) 'values))
			    (cdr default)
			    (list default)))
	(arg-names (mapcar #'car args)))
    (collect ((values-names))
      (dotimes (i values)
	(values-names (gensym)))
      `(progn
	 (define-hash-cache ,name ,args ,@options)
	 (defun ,name ,arg-names
	   ,@decls
	   ,doc
	   (multiple-value-bind
	       ,(values-names)
	       (,(symbolicate name "-CACHE-LOOKUP") ,@arg-names)
	     (if (and ,@(mapcar #'(lambda (val def)
				    `(eq ,val ,def))
				(values-names) default-values))
		 (multiple-value-bind ,(values-names)
				      (progn ,@body)
		   (,(symbolicate name "-CACHE-ENTER") ,@arg-names
		    ,@(values-names))
		   (values ,@(values-names)))
		 (values ,@(values-names)))))))))

;;; CACHE-HASH-EQ  -- Public
;;;
(defmacro cache-hash-eq (x)
  "Return an EQ hash of X.  The value of this hash for any given object can
   (of course) change at arbitary times."
  `(lisp::pointer-hash ,x))
