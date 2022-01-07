;;; -*- Package: KERNEL -*-
;;;
;;; The definitions of most number functions.

(in-package "LISP")

(export '(zerop plusp minusp oddp evenp = /= < > <= >= max min + - * / 1+ 1-
	  conjugate abs phase signum float floor ceiling truncate cis round mod
          rem ffloor fceiling fround ftruncate complex realpart imagpart
	  logior logxor logand logeqv lognand lognor logandc1 logandc2 logorc1
	  logorc2 boole boole-clr boole-set boole-1 boole-2 boole-c1 boole-c2
	  boole-and boole-ior boole-xor boole-eqv boole-nand boole-nor
	  boole-andc1 boole-andc2 boole-orc1 boole-orc2 lognot logcom logtest
	  logbitp ash integer-length byte byte-size byte-position
	  ldb ldb-test mask-field dpb deposit-field
	  upgraded-complex-part-type))

;; FIX consider `integer', like `string', which tries to coerce arg to int
;;        parse-integer for strings, char-code for chars, maybe truncate other nums
;;        type conversion should be automatic?

(in-package "KERNEL")


;;;; Number dispatch macro.

(eval-when (compile load eval)

;;; PARSE-NUMBER-DISPATCH  --  Internal
;;;
;;;    Grovel an individual case to NUMBER-DISPATCH, augmenting Result with the
;;; type dispatches and bodies.  Result is a tree built of alists representing
;;; the dispatching off each arg (in order).  The leaf is the body to be
;;; executed in that case.
;;;
(defun parse-number-dispatch (vars result types var-types body)
  (cond ((null vars)
	 (unless (null types) (error "More types than vars."))
	 (when (cdr result)
	   (error "Duplicate case: ~S." body))
	 (setf (cdr result)
	       (sublis var-types body :test #'equal)))
	((null types)
	 (error "More vars than types."))
	(t
	 (flet ((frob (var type)
		  (parse-number-dispatch
		   (rest vars)
		   (or (assoc type (cdr result) :test #'equal)
		       (car (setf (cdr result)
				  (acons type nil (cdr result)))))
		   (rest types)
		   (acons `(dispatch-type ,var) type var-types)
		   body)))
	   (let ((type (first types))
		 (var (first vars)))
	     (if (and (consp type) (eq (first type) 'foreach))
		 (dolist (type (rest type))
		   (frob var type))
		 (frob var type)))))))

;;; Our guess for the preferred order to do type tests in (cheaper and/or more
;;; probable first.)
;;;
(defconstant type-test-ordering
  '(fixnum single-float double-float integer #+long-float long-float bignum
    complex ratio))

;;; Type-Test-Order  --  Internal
;;;
;;;    Return true if Type1 should be tested before Type2.
;;;
(defun type-test-order (type1 type2)
  (let ((o1 (position type1 type-test-ordering))
	(o2 (position type2 type-test-ordering)))
    (cond ((not o1) nil)
	  ((not o2) t)
	  (t
	   (< o1 o2)))))

;;; GENERATE-NUMBER-DISPATCH  --  Internal
;;;
;;;    Return an ETYPECASE form that does the type dispatch, ordering the cases
;;; for efficiency.
;;;
(defun generate-number-dispatch (vars error-tags cases)
  (if vars
      (let ((var (first vars))
	    (cases (sort cases #'type-test-order :key #'car)))
	`((typecase ,var
	    ,@(mapcar #'(lambda (case)
			  `(,(first case)
			    ,@(generate-number-dispatch (rest vars)
							(rest error-tags)
							(cdr case))))
		      cases)
	    (t (go ,(first error-tags))))))
      cases))

); Eval-When (Compile Load Eval)

;;; NUMBER-DISPATCH  --  Interface
;;;
(defmacro number-dispatch (var-specs &body cases)
  "NUMBER-DISPATCH ({(Var Type)}*) {((Type*) Form*) | (Symbol Arg*)}*
   A vaguely case-like macro that does number cross-product dispatches.  The
   Vars are the variables we are dispatching off of.  The Type paired with each
   Var is used in the error message when no case matches.  Each case specifies a
   Type for each var, and is executed when that signature holds.  A type may be
   a list (FOREACH Each-Type*), causing that case to be repeatedly instantiated
   for every Each-Type.  In the body of each case, any list of the form
   (DISPATCH-TYPE Var-Name) is substituted with the type of that var in that
   instance of the case.

   As an alternate to a case spec, there may be a form whose CAR is a symbol.
   In this case, we apply the CAR of the form to the CDR and treat the result of
   the call as a list of cases.  This process is not applied recursively."
  (let ((res (list nil))
	(vars (mapcar #'car var-specs))
	(block (gensym)))
    (dolist (case cases)
      (if (symbolp (first case))
	  (let ((cases (apply (symbol-function (first case)) (rest case))))
	    (dolist (case cases)
	      (parse-number-dispatch vars res (first case) nil (rest case))))
	  (parse-number-dispatch vars res (first case) nil (rest case))))

    (collect ((errors)
	      (error-tags))
      (dolist (spec var-specs)
	(let ((var (first spec))
	      (type (second spec))
	      (tag (gensym)))
	  (error-tags tag)
	  (errors tag)
	  (errors `(return-from
		    ,block
		    (error 'simple-type-error :datum ,var
			   :expected-type ',type
			   :format-control
			   "Argument ~A is not a ~S: ~S."
			   :format-arguments
			   (list ',var ',type ,var))))))

      `(block ,block
	 (tagbody
	   (return-from ,block
			,@(generate-number-dispatch vars (error-tags)
						    (cdr res)))
	   ,@(errors))))))


;;;; Binary operation dispatching utilities.

(eval-when (compile eval)

;;; FLOAT-CONTAGION  --  Internal
;;;
;;;    Return NUMBER-DISPATCH forms for rational X float.
;;;
(defun float-contagion (op x y &optional (rat-types '(fixnum bignum ratio)))
  `(((single-float single-float) (,op ,x ,y))
    (((foreach ,@rat-types)
      (foreach single-float double-float #+long-float long-float))
     (,op (coerce ,x '(dispatch-type ,y)) ,y))
    (((foreach single-float double-float #+long-float long-float)
      (foreach ,@rat-types))
     (,op ,x (coerce ,y '(dispatch-type ,x))))
    #+long-float
    (((foreach single-float double-float long-float) long-float)
     (,op (coerce ,x 'long-float) ,y))
    #+long-float
    ((long-float (foreach single-float double-float))
     (,op ,x (coerce ,y 'long-float)))
    (((foreach single-float double-float) double-float)
     (,op (coerce ,x 'double-float) ,y))
    ((double-float single-float)
     (,op ,x (coerce ,y 'double-float)))))

;;; BIGNUM-CROSS-FIXNUM  --  Internal
;;;
;;;    Return NUMBER-DISPATCH forms for bignum X fixnum.
;;;
(defun bignum-cross-fixnum (fix-op big-op)
  `(((fixnum fixnum) (,fix-op x y))
    ((fixnum bignum)
     (,big-op (make-small-bignum x) y))
    ((bignum fixnum)
     (,big-op x (make-small-bignum y)))
    ((bignum bignum)
     (,big-op x y))))

); Eval-When (Compile Eval)


;;;; Canonicalization utilities.

;;; CANONICAL-COMPLEX  --  Internal
;;;
;;;    If imagpart is 0, return realpart, otherwise make a complex.  This is
;;; used when we know that realpart and imagpart are the same type, but
;;; rational canonicalization might still need to be done.
;;;
(proclaim '(inline canonical-complex))
(defun canonical-complex (realpart imagpart)
  (if (eql imagpart 0)
      realpart
      (cond #+long-float
	    ((and (typep realpart 'long-float)
		  (typep imagpart 'long-float))
	     (truly-the (complex long-float) (complex realpart imagpart)))
	    ((and (typep realpart 'double-float)
		  (typep imagpart 'double-float))
	     (truly-the (complex double-float) (complex realpart imagpart)))
	    ((and (typep realpart 'single-float)
		  (typep imagpart 'single-float))
	     (truly-the (complex single-float) (complex realpart imagpart)))
	    (t
	     (%make-complex realpart imagpart)))))

;;; BUILD-RATIO  --  Internal
;;;
;;;    Given a numerator and denominator with the GCD already divided out, make
;;; a canonical rational.  We make the denominator positive, and check whether
;;; it is 1.
;;;
(proclaim '(inline build-ratio))
(defun build-ratio (num den)
  (multiple-value-bind (num den)
		       (if (minusp den)
			   (values (- num) (- den))
			   (values num den))
    (if (eql den 1)
	num
	(%make-ratio num den))))

;;; MAYBE-TRUNCATE  --  Internal
;;;
;;;    Truncate X and Y, but bum the case where Y is 1.
;;;
(proclaim '(inline maybe-truncate))
(defun maybe-truncate (x y)
  (if (eql y 1)
      x
      (truncate x y)))


;;;; Complexes.

(defun upgraded-complex-part-type (spec)
  "Returns the element type of the most specialized COMPLEX number type
   that can hold parts of type Spec."
  (cond ((subtypep spec 'single-float)
	 'single-float)
	((subtypep spec 'double-float)
	 'double-float)
	#+long-float
	((subtypep spec 'long-float)
	 'long-float)
	((subtypep spec 'rational)
	 'rational)
	(t)))

(defun complex (realpart &optional (imagpart 0))
  "Builds a complex number from the specified components."
  (flet ((%%make-complex (realpart imagpart)
	   (cond #+long-float
		 ((and (typep realpart 'long-float)
		       (typep imagpart 'long-float))
		  (truly-the (complex long-float)
			     (complex realpart imagpart)))
		 ((and (typep realpart 'double-float)
		       (typep imagpart 'double-float))
		  (truly-the (complex double-float)
			     (complex realpart imagpart)))
		 ((and (typep realpart 'single-float)
		       (typep imagpart 'single-float))
		  (truly-the (complex single-float)
			     (complex realpart imagpart)))
		 (t
		  (%make-complex realpart imagpart)))))
  (number-dispatch ((realpart real) (imagpart real))
    ((rational rational)
     (canonical-complex realpart imagpart))
    (float-contagion %%make-complex realpart imagpart (rational)))))

(defun realpart (number)
  "Extracts the real part of a number."
  (typecase number
    #+long-float
    ((complex long-float)
     (truly-the long-float (realpart number)))
    ((complex double-float)
     (truly-the double-float (realpart number)))
    ((complex single-float)
     (truly-the single-float (realpart number)))
    ((complex rational)
     (kernel:%realpart number))
    (t
     number)))

(defun imagpart (number)
  "Extracts the imaginary part of a number."
  (typecase number
    #+long-float
    ((complex long-float)
     (truly-the long-float (imagpart number)))
    ((complex double-float)
     (truly-the double-float (imagpart number)))
    ((complex single-float)
     (truly-the single-float (imagpart number)))
    ((complex rational)
     (kernel:%imagpart number))
    (float
     (float 0 number))
    (t
     0)))

(defun conjugate (number)
  "Returns the complex conjugate of NUMBER.  For non-complex numbers, this
   is an identity."
  (if (complexp number)
      (complex (realpart number) (- (imagpart number)))
      number))

(defun signum (number)
  "If NUMBER is zero, return NUMBER, else return (/ NUMBER (ABS NUMBER))."
  (if (zerop number)
      number
      (if (rationalp number)
	  (if (plusp number) 1 -1)
	  (/ number (abs number)))))


;;;; Ratios.

(defun numerator (number)
  "Return the numerator of NUMBER, which must be rational."
  (numerator number))

(defun denominator (number)
  "Return the denominator of NUMBER, which must be rational."
  (denominator number))


;;;; Arithmetic Operations

(defmacro define-arith (op init doc)
  `(defun ,op (&rest args)
     ,doc
     (if (null args) ,init
	 (do ((args (cdr args) (cdr args))
	      (res (car args) (,op res (car args))))
	     ((null args) res)))))

(define-arith + 0
  "Returns the sum of its arguments.  With no args, returns 0.")
(define-arith * 1
  "Returns the product of its arguments.  With no args, returns 1.")

(defun - (number &rest more-numbers)
  "Subtracts the second and all subsequent arguments from the first.  With
   one arg, negates it."
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (- result (car nlist))))
      (- number)))

(defun / (number &rest more-numbers)
  "Divides the first arg by each of the following arguments, in turn.  With
   one arg, returns reciprocal."
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (/ result (car nlist))))
      (/ number)))

(defun 1+ (number)
  "Returns NUMBER + 1."
  (1+ number))

(defun 1- (number)
  "Returns NUMBER - 1."
  (1- number))

(eval-when (compile)

(defmacro two-arg-+/- (name op big-op)
  `(defun ,name (x y)
     (number-dispatch ((x number) (y number))
       (bignum-cross-fixnum ,op ,big-op)
       (float-contagion ,op x y)

       ((complex complex)
	(canonical-complex (,op (realpart x) (realpart y))
			   (,op (imagpart x) (imagpart y))))
       (((foreach bignum fixnum ratio single-float double-float
		  #+long-float long-float) complex)
	(complex (,op x (realpart y)) (,op (imagpart y))))
       ((complex (or rational float))
	(complex (,op (realpart x) y) (imagpart x)))

       (((foreach fixnum bignum) ratio)
	(let* ((dy (denominator y))
	       (n (,op (* x dy) (numerator y))))
	  (%make-ratio n dy)))
       ((ratio integer)
	(let* ((dx (denominator x))
	       (n (,op (numerator x) (* y dx))))
	  (%make-ratio n dx)))
       ((ratio ratio)
	(let* ((nx (numerator x))
	       (dx (denominator x))
	       (ny (numerator y))
	       (dy (denominator y))
	       (g1 (gcd dx dy)))
	  (if (eql g1 1)
	      (%make-ratio (,op (* nx dy) (* dx ny)) (* dx dy))
	      (let* ((t1 (,op (* nx (truncate dy g1)) (* (truncate dx g1) ny)))
		     (g2 (gcd t1 g1))
		     (t2 (truncate dx g1)))
		(cond ((eql t1 0) 0)
		      ((eql g2 1)
		       (%make-ratio t1 (* t2 dy)))
		      (T (let* ((nn (truncate t1 g2))
				(t3 (truncate dy g2))
				(nd (if (eql t2 1) t3 (* t2 t3))))
			   (if (eql nd 1) nn (%make-ratio nn nd))))))))))))

); Eval-When (Compile)

(two-arg-+/- two-arg-+ + add-bignums)
(two-arg-+/- two-arg-- - subtract-bignum)

(defun two-arg-* (x y)
  (flet ((integer*ratio (x y)
	   (if (eql x 0) 0
	       (let* ((ny (numerator y))
		      (dy (denominator y))
		      (gcd (gcd x dy)))
		 (if (eql gcd 1)
		     (%make-ratio (* x ny) dy)
		     (let ((nn (* (truncate x gcd) ny))
			   (nd (truncate dy gcd)))
		       (if (eql nd 1)
			   nn
			   (%make-ratio nn nd)))))))
	 (complex*real (x y)
	   (canonical-complex (* (realpart x) y) (* (imagpart x) y))))
    (number-dispatch ((x number) (y number))
      (float-contagion * x y)

      ((fixnum fixnum) (multiply-fixnums x y))
      ((bignum fixnum) (multiply-bignum-and-fixnum x y))
      ((fixnum bignum) (multiply-bignum-and-fixnum y x))
      ((bignum bignum) (multiply-bignums x y))

      ((complex complex)
       (let* ((rx (realpart x))
	      (ix (imagpart x))
	      (ry (realpart y))
	      (iy (imagpart y)))
	 (canonical-complex (- (* rx ry) (* ix iy)) (+ (* rx iy) (* ix ry)))))
      (((foreach bignum fixnum ratio single-float double-float
		 #+long-float long-float)
	complex)
       (complex*real y x))
      ((complex (or rational float))
       (complex*real x y))

      (((foreach bignum fixnum) ratio) (integer*ratio x y))
      ((ratio integer) (integer*ratio y x))
      ((ratio ratio)
       (let* ((nx (numerator x))
	      (dx (denominator x))
	      (ny (numerator y))
	      (dy (denominator y))
	      (g1 (gcd nx dy))
	      (g2 (gcd dx ny)))
	 (build-ratio (* (maybe-truncate nx g1)
			 (maybe-truncate ny g2))
		      (* (maybe-truncate dx g2)
			 (maybe-truncate dy g1))))))))

;;; INTEGER-/-INTEGER  --  Internal
;;;
;;;    Divide two integers, producing a canonical rational.  If a fixnum, we
;;; see if they divide evenly before trying the GCD.  In the bignum case, we
;;; don't bother, since bignum division is expensive, and the test is not very
;;; likely to suceed.
;;;
(defun integer-/-integer (x y)
  (if (and (typep x 'fixnum) (typep y 'fixnum))
      (multiple-value-bind
	  (quo rem)
	  (truncate x y)
	(if (zerop rem)
	    quo
	    (let ((gcd (gcd x y)))
	      (declare (fixnum gcd))
	      (if (eql gcd 1)
		  (build-ratio x y)
		  (build-ratio (truncate x gcd) (truncate y gcd))))))
      (let ((gcd (gcd x y)))
	(if (eql gcd 1)
	    (build-ratio x y)
	    (build-ratio (truncate x gcd) (truncate y gcd))))))

(defun two-arg-/ (x y)
  (number-dispatch ((x number) (y number))
    (float-contagion / x y (ratio integer))

    ((complex complex)
     (let* ((rx (realpart x))
	    (ix (imagpart x))
	    (ry (realpart y))
	    (iy (imagpart y)))
       (if (> (abs ry) (abs iy))
	   (let* ((r (/ iy ry))
		  (dn (* ry (+ 1 (* r r)))))
	     (canonical-complex (/ (+ rx (* ix r)) dn)
				(/ (- ix (* rx r)) dn)))
	   (let* ((r (/ ry iy))
		  (dn (* iy (+ 1 (* r r)))))
	     (canonical-complex (/ (+ (* rx r) ix) dn)
				(/ (- (* ix r) rx) dn))))))
    (((foreach integer ratio single-float double-float) complex)
     (let* ((ry (realpart y))
	    (iy (imagpart y)))
       (if (> (abs ry) (abs iy))
	   (let* ((r (/ iy ry))
		  (dn (* ry (+ 1 (* r r)))))
	     (canonical-complex (/ x dn)
				(/ (- (* x r)) dn)))
	   (let* ((r (/ ry iy))
		  (dn (* iy (+ 1 (* r r)))))
	     (canonical-complex (/ (* x r) dn)
				(/ (- x) dn))))))
    ((complex (or rational float))
     (canonical-complex (/ (realpart x) y)
			(/ (imagpart x) y)))

    ((ratio ratio)
     (let* ((nx (numerator x))
	    (dx (denominator x))
	    (ny (numerator y))
	    (dy (denominator y))
	    (g1 (gcd nx ny))
	    (g2 (gcd dx dy)))
       (build-ratio (* (maybe-truncate nx g1) (maybe-truncate dy g2))
		    (* (maybe-truncate dx g2) (maybe-truncate ny g1)))))

    ((integer integer)
     (integer-/-integer x y))

    ((integer ratio)
     (if (zerop x)
	 0
	 (let* ((ny (numerator y))
		(dy (denominator y))
		(gcd (gcd x ny)))
	   (build-ratio (* (maybe-truncate x gcd) dy)
			(maybe-truncate ny gcd)))))

    ((ratio integer)
     (let* ((nx (numerator x))
	    (gcd (gcd nx y)))
       (build-ratio (maybe-truncate nx gcd)
		    (* (maybe-truncate y gcd) (denominator x)))))))

(defun %negate (n)
  (number-dispatch ((n number))
    (((foreach fixnum single-float double-float #+long-float long-float))
     (%negate n))
    ((bignum)
     (negate-bignum n))
    ((ratio)
     (%make-ratio (- (numerator n)) (denominator n)))
    ((complex)
     (complex (- (realpart n)) (- (imagpart n))))))


;;;; Truncate and friends.

(defun truncate (number &optional (divisor 1))
  "Returns number (or number/divisor) as an integer, rounded toward 0.  The
   second returned value is the remainder."
  (macrolet ((truncate-float (rtype)
	       `(let* ((float-div (coerce divisor ',rtype))
		       (res (%unary-truncate (/ number float-div))))
		  (values res
			  (- number
			     (* (coerce res ',rtype) float-div))))))
    (number-dispatch ((number real) (divisor real))
      ((fixnum fixnum) (truncate number divisor))
      (((foreach fixnum bignum) ratio)
       (let ((q (truncate (* number (denominator divisor))
			  (numerator divisor))))
	 (values q (- number (* q divisor)))))
      ((fixnum bignum)
       (values 0 number))
      ((ratio (or float rational))
       (let ((q (truncate (numerator number)
			  (* (denominator number) divisor))))
	 (values q (- number (* q divisor)))))
      ((bignum fixnum)
       (bignum-truncate number (make-small-bignum divisor)))
      ((bignum bignum)
       (bignum-truncate number divisor))

      (((foreach single-float double-float #+long-float long-float)
	(or rational single-float))
       (if (eql divisor 1)
	   (let ((res (%unary-truncate number)))
	     (values res (- number (coerce res '(dispatch-type number)))))
	   (truncate-float (dispatch-type number))))
      #+long-float
      ((long-float (or single-float double-float long-float))
       (truncate-float long-float))
      #+long-float
      (((foreach double-float single-float) long-float)
       (truncate-float long-float))
      ((double-float (or single-float double-float))
       (truncate-float double-float))
      ((single-float double-float)
       (truncate-float double-float))
      (((foreach fixnum bignum ratio)
	(foreach single-float double-float #+long-float long-float))
       (truncate-float (dispatch-type divisor))))))

;;; Declare these guys inline to let them get optimized a little.  Round and
;;; Fround are not declared inline since they seem too obscure and too
;;; big to inline-expand by default.  Also, this gives the compiler a chance to
;;; pick off the unary float case.  Simlarly, ceiling and floor are only
;;; maybe-inline for now, so that the power-of-2 ceiling and floor transforms
;;; get a chance.
;;;
(declaim (inline rem mod fceiling ffloor ftruncate))
(declaim (maybe-inline ceiling floor))

;;; If the numbers do not divide exactly and the result of (/ number divisor)
;;; would be negative then decrement the quotient and augment the remainder by
;;; the divisor.
;;;
(defun floor (number &optional (divisor 1))
  "Returns the greatest integer not greater than number, or number/divisor.
   The second returned value is (mod number divisor)."
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(values (1- tru) (+ rem divisor))
	(values tru rem))))

;;; If the numbers do not divide exactly and the result of (/ number divisor)
;;; would be positive then increment the quotient and decrement the remainder by
;;; the divisor.
;;;
(defun ceiling (number &optional (divisor 1))
  "Returns the smallest integer not less than number, or number/divisor.
   The second returned value is the remainder."
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (minusp number)
		 (plusp number)))
	(values (+ tru 1) (- rem divisor))
	(values tru rem))))

(defun round (number &optional (divisor 1))
  "Rounds number (or number/divisor) to nearest integer.  The second
   returned value is the remainder."
  (if (eql divisor 1)
      (round number)
      (multiple-value-bind (tru rem) (truncate number divisor)
	(let ((thresh (/ (abs divisor) 2)))
	  (cond ((or (> rem thresh)
		     (and (= rem thresh) (oddp tru)))
		 (if (minusp divisor)
		     (values (- tru 1) (+ rem divisor))
		     (values (+ tru 1) (- rem divisor))))
		((let ((-thresh (- thresh)))
		   (or (< rem -thresh)
		       (and (= rem -thresh) (oddp tru))))
		 (if (minusp divisor)
		     (values (+ tru 1) (- rem divisor))
		     (values (- tru 1) (+ rem divisor))))
		(t (values tru rem)))))))

(defun rem (number divisor)
  "Returns second result of TRUNCATE."
  (multiple-value-bind (tru rem) (truncate number divisor)
    (declare (ignore tru))
    rem))

(defun mod (number divisor)
  "Returns second result of FLOOR."
  (let ((rem (rem number divisor)))
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(+ rem divisor)
	rem)))

(macrolet ((frob (name op doc)
	     `(defun ,name (number &optional (divisor 1))
		,doc
		(multiple-value-bind (res rem) (,op number divisor)
		  (values (float res (if (floatp rem) rem 1.0)) rem)))))
  (frob ffloor floor
    "Same as FLOOR, but returns first value as a float.")
  (frob fceiling ceiling
    "Same as CEILING, but returns first value as a float." )
  (frob ftruncate truncate
    "Same as TRUNCATE, but returns first value as a float.")
  (frob fround round
    "Same as ROUND, but returns first value as a float."))


;;;; Comparisons.

(defun = (number &rest more-numbers)
  "Returns T if all of its arguments are numerically equal, NIL otherwise."
  (do ((nlist more-numbers (cdr nlist)))
      ((atom nlist) T)
     (declare (list nlist))
     (if (not (= (car nlist) number)) (return nil))))

(defun /= (number &rest more-numbers)
  "Returns T if no two of its arguments are numerically equal, NIL
   otherwise."
  (do* ((head number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (unless (do* ((nl nlist (cdr nl)))
		  ((atom nl) T)
	       (declare (list nl))
	       (if (= head (car nl)) (return nil)))
       (return nil))))

(defun < (number &rest more-numbers)
  "Returns T if its arguments are in strictly increasing order, NIL
   otherwise."
  (do* ((n number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
    (declare (list nlist))
    (if (not (< n (car nlist))) (return nil))))

(defun > (number &rest more-numbers)
  "Returns T if its arguments are in strictly decreasing order, NIL
   otherwise."
  (do* ((n number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (if (not (> n (car nlist))) (return nil))))

(defun <= (number &rest more-numbers)
  "Returns T if arguments are in strictly non-decreasing order, NIL
   otherwise."
  (do* ((n number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (if (not (<= n (car nlist))) (return nil))))

(defun >= (number &rest more-numbers)
  "Returns T if arguments are in strictly non-increasing order, NIL
   otherwise."
  (do* ((n number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (if (not (>= n (car nlist))) (return nil))))

(defun max (number &rest more-numbers)
  "Returns the greatest of its arguments."
  (do ((nlist more-numbers (cdr nlist))
       (result number))
      ((null nlist) (return result))
     (declare (list nlist))
     (if (> (car nlist) result) (setq result (car nlist)))))

(defun min (number &rest more-numbers)
  "Returns the least of its arguments."
  (do ((nlist more-numbers (cdr nlist))
       (result number))
      ((null nlist) (return result))
     (declare (list nlist))
     (if (< (car nlist) result) (setq result (car nlist)))))

(eval-when (compile eval)

(defun basic-compare (op)
  `(((fixnum fixnum) (,op x y))

    ((single-float single-float) (,op x y))
    #+long-float
    (((foreach single-float double-float long-float) long-float)
     (,op (coerce x 'long-float) y))
    #+long-float
    ((long-float (foreach single-float double-float))
     (,op x (coerce y 'long-float)))
    (((foreach single-float double-float) double-float)
     (,op (coerce x 'double-float) y))
    ((double-float single-float)
     (,op x (coerce y 'double-float)))
    (((foreach single-float double-float #+long-float long-float) rational)
     (if (eql y 0)
	 (,op x (coerce 0 '(dispatch-type x)))
	 (,op (rational x) y)))
    (((foreach bignum fixnum ratio) float)
     (,op x (rational y)))))

(defmacro two-arg-</> (name op ratio-arg1 ratio-arg2 &rest cases)
  `(defun ,name (x y)
     (number-dispatch ((x real) (y real))
       (basic-compare ,op)

       (((foreach fixnum bignum) ratio)
	(,op x (,ratio-arg2 (numerator y) (denominator y))))
       ((ratio integer)
	(,op (,ratio-arg1 (numerator x) (denominator x)) y))
       ((ratio ratio)
	(,op (* (numerator (truly-the ratio x))
		(denominator (truly-the ratio y)))
	     (* (numerator (truly-the ratio y))
		(denominator (truly-the ratio x)))))
       ,@cases)))

); Eval-When (Compile Eval)

(two-arg-</> two-arg-< < floor ceiling
	     ((fixnum bignum)
	      (bignum-plus-p y))
	     ((bignum fixnum)
	      (not (bignum-plus-p x)))
	     ((bignum bignum)
	      (minusp (bignum-compare x y))))

(two-arg-</> two-arg-> > ceiling floor
	     ((fixnum bignum)
	      (not (bignum-plus-p y)))
	     ((bignum fixnum)
	      (bignum-plus-p x))
	     ((bignum bignum)
	      (plusp (bignum-compare x y))))

(defun two-arg-= (x y)
  (number-dispatch ((x number) (y number))
    (basic-compare =)

    ((fixnum (or bignum ratio)) nil)

    ((bignum (or fixnum ratio)) nil)
    ((bignum bignum)
     (zerop (bignum-compare x y)))

    ((ratio integer) nil)
    ((ratio ratio)
     (and (eql (numerator x) (numerator y))
	  (eql (denominator x) (denominator y))))

    ((complex complex)
     (and (= (realpart x) (realpart y))
	  (= (imagpart x) (imagpart y))))
    (((foreach fixnum bignum ratio single-float double-float
	       #+long-float long-float) complex)
     (and (= x (realpart y))
	  (zerop (imagpart y))))
    ((complex (or float rational))
     (and (= (realpart x) y)
	  (zerop (imagpart x))))))

;;; EQL -- Public
;;;
(defun eql (obj1 obj2)
  "Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL."
  (or (eq obj1 obj2)
      (if (or (typep obj2 'fixnum)
	      (not (typep obj2 'number)))
	  nil
	  (macrolet ((foo (&rest stuff)
		       `(typecase obj2
			  ,@(mapcar #'(lambda (foo)
					(let ((type (car foo))
					      (fn (cadr foo)))
					  `(,type
					    (and (typep obj1 ',type)
						 (,fn obj1 obj2)))))
				    stuff))))
	    (foo
	      (single-float eql)
	      (double-float eql)
	      #+long-float
	      (long-float eql)
	      (bignum
	       (lambda (x y)
		 (zerop (bignum-compare x y))))
	      (ratio
	       (lambda (x y)
		 (and (eql (numerator x) (numerator y))
		      (eql (denominator x) (denominator y)))))
	      (complex
	       (lambda (x y)
		 (and (eql (realpart x) (realpart y))
		      (eql (imagpart x) (imagpart y))))))))))


;;;; Logicals.

(defun logior (&rest integers)
  "Returns the bit-wise or of its arguments.  Args must be integers."
  (declare (list integers))
  (if integers
      (do ((result (pop integers) (logior result (pop integers))))
	  ((null integers) result))
      0))

(defun logxor (&rest integers)
  "Returns the bit-wise exclusive or of its arguments.  Args must be
   integers."
  (declare (list integers))
  (if integers
      (do ((result (pop integers) (logxor result (pop integers))))
	  ((null integers) result))
      0))

(defun logand (&rest integers)
  "Returns the bit-wise and of its arguments.  Args must be integers."
  (declare (list integers))
  (if integers
      (do ((result (pop integers) (logand result (pop integers))))
	  ((null integers) result))
      -1))

(defun logeqv (&rest integers)
  "Returns the bit-wise equivalence of its arguments.  Args must be
   integers."
  (declare (list integers))
  (if integers
      (do ((result (pop integers) (logeqv result (pop integers))))
	  ((null integers) result))
      -1))

(defun lognand (integer1 integer2)
  "Returns the complement of the logical AND of integer1 and integer2."
  (lognand integer1 integer2))

(defun lognor (integer1 integer2)
  "Returns the complement of the logical OR of integer1 and integer2."
  (lognor integer1 integer2))

(defun logandc1 (integer1 integer2)
  "Returns the logical AND of (LOGNOT integer1) and integer2."
  (logandc1 integer1 integer2))

(defun logandc2 (integer1 integer2)
  "Returns the logical AND of integer1 and (LOGNOT integer2)."
  (logandc2 integer1 integer2))

(defun logorc1 (integer1 integer2)
  "Returns the logical OR of (LOGNOT integer1) and integer2."
  (logorc1 integer1 integer2))

(defun logorc2 (integer1 integer2)
  "Returns the logical OR of integer1 and (LOGNOT integer2)."
  (logorc2 integer1 integer2))

;; FIX
(defun lognot (number)
  "Returns the bit-wise logical complement of Number."
  (etypecase number
    (fixnum (lognot (truly-the fixnum number)))
    (bignum (bignum-logical-not number))))

(defun logcom (number)
  "Returns the bit-wise logical complement of Number."
  (etypecase number
    (fixnum (lognot (truly-the fixnum number)))
    (bignum (bignum-logical-not number))))

(macrolet ((frob (name op big-op)
	     `(defun ,name (x y)
	       (number-dispatch ((x integer) (y integer))
		 (bignum-cross-fixnum ,op ,big-op)))))
  (frob two-arg-and logand bignum-logical-and)
  (frob two-arg-ior logior bignum-logical-ior)
  (frob two-arg-xor logxor bignum-logical-xor))

(defun logcount (integer)
  "Count the number of 1 bits if INTEGER is positive, and the number of 0
   bits if INTEGER is negative."
  (etypecase integer
    (fixnum
     (logcount (truly-the (integer 0 #.(max most-positive-fixnum
					    (lognot most-negative-fixnum)))
			  (if (minusp (truly-the fixnum integer))
			      (lognot (truly-the fixnum integer))
			      integer))))
    (bignum
     (bignum-logcount integer))))

(defun logtest (integer1 integer2)
  "Predicate which returns T if logand of integer1 and integer2 is not
   zero."
  (logtest integer1 integer2))

(defun logbitp (index integer)
  "Predicate returns T if bit index of integer is a 1.  The least
   significant bit of INTEGER is bit 0."
  (etypecase integer
    (fixnum (logbitp index integer))
    (bignum (bignum-logbitp index integer))))

(defun ash (integer count)
  "Shifts integer left by count places preserving sign.  - count shifts
   right."
  (declare (integer integer count))
  (etypecase integer
    (fixnum
     (cond ((zerop integer)
	    0)
	   ((fixnump count)
	    (let ((length (integer-length (truly-the fixnum integer)))
		  (count (truly-the fixnum count)))
	      (declare (fixnum length count))
	      (cond ((and (plusp count)
			  (> (+ length count)
			     (integer-length most-positive-fixnum)))
		     (bignum-ashift-left (make-small-bignum integer) count))
		    (t
		     (truly-the fixnum
				(ash (truly-the fixnum integer) count))))))
	   ((minusp count)
	    (if (minusp integer) -1 0))
	   (t
	    (bignum-ashift-left (make-small-bignum integer) count))))
    (bignum
     (if (plusp count)
	 (bignum-ashift-left integer count)
	 (bignum-ashift-right integer (- count))))))

(defun integer-length (integer)
  "Returns the number of significant bits in the absolute value of
   integer."
  (etypecase integer
    (fixnum
     (integer-length (truly-the fixnum integer)))
    (bignum
     (bignum-integer-length integer))))


;;;; Byte operations.

(defun byte (size position)
  "Returns a byte specifier which may be used by other byte functions."
  (byte size position))

(defun byte-size (bytespec)
  "Returns the size part of the byte specifier bytespec."
  (byte-size bytespec))

(defun byte-position (bytespec)
  "Returns the position part of the byte specifier bytespec."
  (byte-position bytespec))

(defun ldb (bytespec integer)
  "Extract the specified byte from integer, and right justify result."
  (ldb bytespec integer))

(defun ldb-test (bytespec integer)
  "Returns T if any of the specified bits in integer are 1's."
  (ldb-test bytespec integer))

(defun mask-field (bytespec integer)
  "Extract the specified byte from integer, but do not right justify
   result."
  (mask-field bytespec integer))

(defun dpb (newbyte bytespec integer)
  "Returns new integer with newbyte in specified position, newbyte is right
   justified."
  (dpb newbyte bytespec integer))

(defun deposit-field (newbyte bytespec integer)
  "Returns new integer with newbyte in specified position, newbyte is not
   right justified."
  (deposit-field newbyte bytespec integer))

(defun %ldb (size posn integer)
  (logand (ash integer (- posn))
	  (1- (ash 1 size))))

(defun %mask-field (size posn integer)
  (logand integer (ash (1- (ash 1 size)) posn)))

(defun %dpb (newbyte size posn integer)
  (let ((mask (1- (ash 1 size))))
    (logior (logand integer (lognot (ash mask posn)))
	    (ash (logand newbyte mask) posn))))

(defun %deposit-field (newbyte size posn integer)
  (let ((mask (ash (ldb (byte size 0) -1) posn)))
    (logior (logand newbyte mask)
	    (logand integer (lognot mask)))))


;;;; Boole.

;;; The boole function dispaches to any logic operation depending on the
;;; value of a variable.  Presently, legal selector values are [0..15].
;;; boole is open coded for calls with a constant selector. or with calls
;;; using any of the constants declared below.

(defconstant boole-clr 0
  "Boole function op, makes BOOLE return 0.")

(defconstant boole-set 1
  "Boole function op, makes BOOLE return -1.")

(defconstant boole-1   2
  "Boole function op, makes BOOLE return integer1.")

(defconstant boole-2   3
  "Boole function op, makes BOOLE return integer2.")

(defconstant boole-c1  4
  "Boole function op, makes BOOLE return complement of integer1.")

(defconstant boole-c2  5
  "Boole function op, makes BOOLE return complement of integer2.")

(defconstant boole-and 6
  "Boole function op, makes BOOLE return logand of integer1 and integer2.")

(defconstant boole-ior 7
  "Boole function op, makes BOOLE return logior of integer1 and integer2.")

(defconstant boole-xor 8
  "Boole function op, makes BOOLE return logxor of integer1 and integer2.")

(defconstant boole-eqv 9
  "Boole function op, makes BOOLE return logeqv of integer1 and integer2.")

(defconstant boole-nand  10
  "Boole function op, makes BOOLE return log nand of integer1 and
   integer2.")

(defconstant boole-nor   11
  "Boole function op, makes BOOLE return lognor of integer1 and integer2.")

(defconstant boole-andc1 12
  "Boole function op, makes BOOLE return logandc1 of integer1 and
   integer2.")

(defconstant boole-andc2 13
  "Boole function op, makes BOOLE return logandc2 of integer1 and
   integer2.")

(defconstant boole-orc1  14
  "Boole function op, makes BOOLE return logorc1 of integer1 and integer2.")

(defconstant boole-orc2  15
  "Boole function op, makes BOOLE return logorc2 of integer1 and integer2.")

(defun boole (op integer1 integer2)
  "Bit-wise boolean function on two integers.  Function chosen by OP:
	0	BOOLE-CLR
	1	BOOLE-SET
	2	BOOLE-1
  	3	BOOLE-2
	4	BOOLE-C1
	5	BOOLE-C2
	6	BOOLE-AND
	7	BOOLE-IOR
 	8	BOOLE-XOR
	9	BOOLE-EQV
	10	BOOLE-NAND
	11	BOOLE-NOR
	12	BOOLE-ANDC1
	13	BOOLE-ANDC2
	14	BOOLE-ORC1
	15	BOOLE-ORC2"
  (case op
    (0 (boole 0 integer1 integer2))
    (1 (boole 1 integer1 integer2))
    (2 (boole 2 integer1 integer2))
    (3 (boole 3 integer1 integer2))
    (4 (boole 4 integer1 integer2))
    (5 (boole 5 integer1 integer2))
    (6 (boole 6 integer1 integer2))
    (7 (boole 7 integer1 integer2))
    (8 (boole 8 integer1 integer2))
    (9 (boole 9 integer1 integer2))
    (10 (boole 10 integer1 integer2))
    (11 (boole 11 integer1 integer2))
    (12 (boole 12 integer1 integer2))
    (13 (boole 13 integer1 integer2))
    (14 (boole 14 integer1 integer2))
    (15 (boole 15 integer1 integer2))
    (t (error "~S is not of type (mod 16)." op))))


;;;; GCD, LCM.

(defun gcd (&rest numbers)
  "Returns the greatest common divisor of the arguments, which must be
   integers.  Gcd with no arguments is defined to be 0."
  (cond ((null numbers) 0)
	((null (cdr numbers)) (abs (the integer (car numbers))))
	(t
	 (do ((gcd (the integer (car numbers))
		   (gcd gcd (the integer (car rest))))
	      (rest (cdr numbers) (cdr rest)))
	     ((null rest) gcd)
	   (declare (integer gcd)
		    (list rest))))))

(defun lcm (&rest numbers)
  "Returns the least common multiple of one or more integers.  LCM of no
   arguments is defined to be 1."
  (cond ((null numbers) 1)
	((null (cdr numbers)) (abs (the integer (car numbers))))
	(t
	 (do ((lcm (the integer (car numbers))
		   (lcm lcm (the integer (car rest))))
	      (rest (cdr numbers) (cdr rest)))
	     ((null rest) lcm)
	   (declare (integer lcm) (list rest))))))

(defun two-arg-lcm (n m)
  (declare (integer n m))
  (* (truncate (max n m) (gcd n m)) (min n m)))

;;; TWO-ARG-GCD  --  Internal
;;;
;;;    Do the GCD of two integer arguments.  With fixnum arguments, we use the
;;; binary GCD algorithm from Knuth's seminumerical algorithms (slightly
;;; structurified), otherwise we call BIGNUM-GCD.  We pick off the special case
;;; of 0 before the dispatch so that the bignum code doesn't have to worry
;;; about "small bignum" zeros.
;;;
(defun two-arg-gcd (u v)
  (cond ((eql u 0) v)
	((eql v 0) u)
	(t
	 (number-dispatch ((u integer) (v integer))
	   ((fixnum fixnum)
	    (locally
	      (declare (optimize (speed 3) (safety 0)))
	      (do ((k 0 (1+ k))
		   (u (abs u) (ash u -1))
		   (v (abs v) (ash v -1)))
		  ((oddp (logior u v))
		   (do ((temp (if (oddp u) (- v) (ash u -1))
			      (ash temp -1)))
		       (nil)
		     (declare (fixnum temp))
		     (when (oddp temp)
		       (if (plusp temp)
			   (setq u temp)
			   (setq v (- temp)))
		       (setq temp (- u v))
		       (when (zerop temp)
			 (let ((res (ash u k)))
			   (declare (type (signed-byte 31) res)
				    (optimize (inhibit-warnings 3)))
			   (return res))))))
		(declare (type (mod 30) k)
			 (type (signed-byte 31) u v)))))
	   ((bignum bignum)
	    (bignum-gcd u v))
	   ((bignum fixnum)
	    (bignum-gcd u (make-small-bignum v)))
	   ((fixnum bignum)
	    (bignum-gcd (make-small-bignum u) v))))))


;;; Primep  --  Public
;;;
(defun primep (x)
  "Returns T iff X is a positive prime integer."
  (declare (integer x))
  (if (<= x 5)
      (and (>= x 2) (/= x 4))
      (and (not (evenp x))
	   (not (zerop (rem x 3)))
	   (do ((q 6)
		(r 1)
		(inc 2 (logxor inc 6)) ;; 2,4,2,4...
		(d 5 (+ d inc)))
	       ((or (= r 0) (> d q)) (/= r 0))
	     (declare (fixnum inc))
	     (multiple-value-setq (q r) (truncate x d))))))


;;; ISQRT  --  Public
;;;
;;;    From discussion on comp.lang.lisp and Akira Kurihara.
;;;
(defun isqrt (n)
  "Returns the root of the nearest integer less than n which is a perfect
   square."
  (declare (type unsigned-byte n) (values unsigned-byte))
  ;; theoretically (> n 7) ,i.e., n-len-quarter > 0
  (if (and (fixnump n) (<= n 24))
      (cond ((> n 15) 4)
	    ((> n  8) 3)
	    ((> n  3) 2)
	    ((> n  0) 1)
	    (t 0))
      (let* ((n-len-quarter (ash (integer-length n) -2))
	     (n-half (ash n (- (ash n-len-quarter 1))))
	     (n-half-isqrt (isqrt n-half))
	     (init-value (ash (1+ n-half-isqrt) n-len-quarter)))
	(loop
	  (let ((iterated-value
		 (ash (+ init-value (truncate n init-value)) -1)))
	    (unless (< iterated-value init-value)
	      (return init-value))
	    (setq init-value iterated-value))))))


;;;; Miscellaneous number predicates.

(defun zerop (number)
  "Returns t if NUMBER = 0, () otherwise."
  (zerop number))

(defun plusp (number)
  "Returns t if NUMBER > 0, () otherwise."
  (plusp number))

(defun minusp (number)
  "Returns t if NUMBER < 0, () otherwise."
  (minusp number))

(defun oddp (number)
  "Returns t if NUMBER is odd, () otherwise."
  (oddp number))

(defun evenp (number)
  "Returns t if NUMBER is even, () otherwise."
  (evenp number))
