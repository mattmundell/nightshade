;;; Functions to implement lists.
;;;
;;; FIX Nsublis, things at the beginning broken.

(in-package "LISP")

(export '(car cdr caar
	  cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
	  cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr
	  caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr
	  cdddar cddddr cons tree-equal endp list-length nth first
	  second third fourth fifth sixth seventh eighth
	  ninth tenth rest nthcdr last list list* make-list
	  append copy-list copy-alist copy-tree revappend nconc
	  nreconc butlast nbutlast ldiff rplaca rplacd subst
	  subst-if subst-if-not nsubst nsubst-if nsubst-if-not sublis nsublis
	  member member-if member-if-not tailp adjoin union
	  nunion intersection nintersection set-difference
	  nset-difference set-exclusive-or nset-exclusive-or subsetp
	  acons pairlis
	  assoc assoc= assoc-if assoc-if-not
	  rassoc rassoc= rassoc-if rassoc-if-not
	  complement constantly
	  identity t
	  duplicatesp))

(proclaim '(maybe-inline
	    tree-equal list-length nth %setnth nthcdr last make-list append
	    copy-list copy-alist copy-tree revappend nconc nreconc butlast
	    nbutlast ldiff member member-if member-if-not tailp adjoin union
	    nunion intersection nintersection set-difference nset-difference
	    set-exclusive-or nset-exclusive-or subsetp acons pairlis assoc
	    assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not subst subst-if
	    subst-if-not nsubst nsubst-if nsubst-if-not sublis nsublis))

(in-package "EXTENSIONS")
(export '(assq memq delq))
(proclaim '(maybe-inline delq))
(in-package "LISP")

#[ List

cons  car cdr
cdr can be a cons

()  empty list

read syntax
  (1 . 2)
  (1 2 3)
  (1 . (2 . (3 . ())))

print syntax
]#

;;; These functions perform basic list operations:
(defun car (list) "Return the first item in $list." (car list))
(defun cdr (list) "Return all but the first item." (cdr list))
(defun cadr (list) "Return the second item in $list." (cadr list))
(defun cdar (list) "Return the cdr of the first sublist." (cdar list))
(defun caar (list) "Return the car of the first sublist." (caar list))
(defun cddr (list) "Return all but the first two items of $list." (cddr list))
(defun caddr (list) "Return the first item in the cddr of $list." (caddr list))
(defun caadr (list) "Return the first item in the cadr of $list." (caadr list))
(defun caaar (list) "Return the first item in the caar of $list." (caaar list))
(defun cdaar (list) "Return the cdr of the caar of $list." (cdaar list))
(defun cddar (list) "Return the cdr of the cdar of $list." (cddar list))
(defun cdddr (list) "Return the cdr of the cddr of $list." (cdddr list))
(defun cadar (list) "Return the car of the cdar of $list." (cadar list))
(defun cdadr (list) "Return the cdr of the cadr of $list." (cdadr list))
(defun caaaar (list) "Return the car of the caaar of $list." (caaaar list))
(defun caaadr (list) "Return the car of the caadr of $list." (caaadr list))
(defun caaddr (list) "Return the car of the caddr of $list." (caaddr list))
(defun cadddr (list) "Return the car of the cdddr of $list." (cadddr list))
(defun cddddr (list) "Return the cdr of the cdddr of $list." (cddddr list))
(defun cdaaar (list) "Return the cdr of the caaar of $list." (cdaaar list))
(defun cddaar (list) "Return the cdr of the cdaar of $list." (cddaar list))
(defun cdddar (list) "Return the cdr of the cddar of $list." (cdddar list))
(defun caadar (list) "Return the car of the cadar of $list." (caadar list))
(defun cadaar (list) "Return the car of the cdaar of $list." (cadaar list))
(defun cadadr (list) "Return the car of the cdadr of $list." (cadadr list))
(defun caddar (list) "Return the car of the cddar of $list." (caddar list))
(defun cdaadr (list) "Return the cdr of the caadr of $list." (cdaadr list))
(defun cdadar (list) "Return the cdr of the cadar of $list." (cdadar list))
(defun cdaddr (list) "Return the cdr of the caddr of $list." (cdaddr list))
(defun cddadr (list) "Return the cdr of the cdadr of $list." (cddadr list))
(defun cons (se1 se2) "Return a list with se1 as the car and se2 as the cdr."
                      (cons se1 se2))


(proclaim '(maybe-inline tree-equal-test tree-equal-test-not))

(defun tree-equal-test-not (x y test-not)
  (cond ((consp x)
	 (and (consp y)
	      (tree-equal-test-not (car x) (car y) test-not)
	      (tree-equal-test-not (cdr x) (cdr y) test-not)))
	((consp y) nil)
	((not (funcall test-not x y)) t)
	(t ())))

(defun tree-equal-test (x y test)
  (cond	((consp x)
	 (and (consp y)
	      (tree-equal-test (car x) (car y) test)
	      (tree-equal-test (cdr x) (cdr y) test)))
	((consp y) nil)
	((funcall test x y) t)
	(t ())))

(defun tree-equal (x y &key (test #'eql) test-not)
  "Return T if X and Y are isomorphic trees with identical leaves."
  (if test-not
      (tree-equal-test-not x y test-not)
      (tree-equal-test x y test)))

(defun endp (item)
  "The recommended way to test for the end of a list.  True if $item is
   nil, false if $item is a cons, and an error for any other types of
   arguments."
  (endp item))

(defun list-length (list)
  "Return the length of the given List, or Nil if the List is circular."
  (do ((n 0 (+ n 2))
       (y list (cddr y))
       (z list (cdr z)))
      (())
    (declare (fixnum n) (list y z))
    (when (endp y) (return n))
    (when (endp (cdr y)) (return (+ n 1)))
    (when (and (eq y z) (> n 0)) (return nil))))

(defun nth (n list)
  "Return the $n'th item in $list where the car is the zero-th item."
  (car (nthcdr n list)))

(defun first (list)
  "Return the first item in $list or NIL if the list is empty."
  (car list))
(defun second (list)
  "Return the second item in $list or NIL if there are fewer items."
  (cadr list))
(defun third (list)
  "Return the third item in $list or NIL if there are fewer items."
  (caddr list))
(defun fourth (list)
  "Return the fourth item in $list or NIL if there are fewer items."
  (cadddr list))
(defun fifth (list)
  "Return the fifth item in $list or NIL if there are fewer items."
  (car (cddddr list)))
(defun sixth (list)
  "Return the sixth item in $list or NIL if there are fewer items."
  (cadr (cddddr list)))
(defun seventh (list)
  "Return the seventh item in $list or NIL if there are fewer items."
  (caddr (cddddr list)))
(defun eighth (list)
  "Return the eighth item in $list or NIL if there are fewer items."
  (cadddr (cddddr list)))
(defun ninth (list)
  "Return the ninth item in $list or NIL if there are fewer items."
  (car (cddddr (cddddr list))))
(defun tenth (list)
  "Return the tenth item in $list or NIL if there are fewer items."
  (cadr (cddddr (cddddr list))))
(defun rest (list)
  "Means the same as the `cdr' of $list."
  (cdr list))

(defun nthcdr (n list)
  (declare (type index n))
  "Performs the `cdr' function `n' times on $list."
  (do ((i n (1- i))
       (result list (cdr result)))
      ((not (plusp i)) result)
      (declare (type index i))))

(defun last (list &optional (n 1))
  "Return the last $n conses of $list."
  (declare (type index n))
  (do ((checked-list list (cdr checked-list))
       (returned-list list)
       (index 0 (1+ index)))
      ((atom checked-list) returned-list)
    (declare (type index index))
    (if (>= index n)
	(pop returned-list))))

(defun list (&rest args)
  "Construct and return a list $args."
  args)

;;; List* is done the same as list, except that the last cons is made a
;;; dotted pair

(defun list* (arg &rest others)
  "Return a list of the arguments with last cons a dotted pair"
  (cond ((atom others) arg)
	((atom (cdr others)) (cons arg (car others)))
	(t (do ((x others (cdr x)))
	       ((null (cddr x)) (rplacd x (cadr x))))
	   (cons arg others))))

(defun make-list (size &key initial-element)
  "Constructs a list with size elements each set to value"
  (declare (type index size))
  (do ((count size (1- count))
       (result '() (cons initial-element result)))
      ((zerop count) result)
    (declare (type index count))))


;;; The outer loop finds the first non-null list and the result is started.
;;; The remaining lists in the arguments are tacked to the end of the result
;;; using splice which cdr's down the end of the new list

(defun append (&rest lists)
  "Construct a new list by concatenating the list arguments."
  (do ((top lists (cdr top)))	 ; Cdr to first non-null list.
      ((atom top) '())
    (cond ((null (car top)))				; Nil -> Keep looping
	  ((not (consp (car top)))			; Non cons
	   (if (cdr top)
	       (error "~S is not a list." (car top))
	       (return (car top))))
	  (t						; Start appending
	   (return
	     (if (atom (cdr top))
		 (car top)       ; Special case.
		 (let* ((result (cons (caar top) '()))
			(splice result))
		   (do ((x (cdar top) (cdr x)))  ; Copy first list
		       ((atom x))
		     (setq splice
			   (cdr (rplacd splice (cons (car x) ()) ))) )
		   (do ((y (cdr top) (cdr y)))	 ; Copy rest of lists.
		       ((atom (cdr y))
			(setq splice (rplacd splice (car y)))
			result)
		     (if (listp (car y))
			 (do ((x (car y) (cdr x)))   ; Inner copy loop.
			     ((atom x))
			   (setq
			    splice
			    (cdr (rplacd splice (cons (car x) ())))))
			 (error "~S is not a list." (car y)))))))))))


;;; List Copying Functions

;;; The list is copied correctly even if the list is not terminated by ()
;;; The new list is built by cdr'ing splice which is always at the tail
;;; of the new list

(defun copy-list (list)
  "Return a new list EQUAL but not EQ to list"
  (if (atom list)
      list
      (let ((result (list (car list))))
	(do ((x (cdr list) (cdr x))
	     (splice result
		     (cdr (rplacd splice (cons (car x) '() ))) ))
	    ((atom x)
	     (unless (null x)
	       (rplacd splice x))))
	result)))

(defun copy-alist (alist)
  "Return a new association list equal to alist, constructed in space"
  (if (atom alist)
      alist
      (let ((result
	     (cons (if (atom (car alist))
		       (car alist)
		       (cons (caar alist) (cdar alist)) )
		   nil)))
	(do ((x (cdr alist) (cdr x))
	     (splice result
		     (cdr (rplacd splice
				  (cons
				   (if (atom (car x))
				       (car x)
				       (cons (caar x) (cdar x)))
				   nil)))))
	    ;; Non-null terminated alist done here.
	    ((atom x)
	     (unless (null x)
	       (rplacd splice x))))
	result)))

(defun copy-tree (object)
  "Copy-Tree recursively copys trees of conses."
  (if (consp object)
      (cons (copy-tree (car object)) (copy-tree (cdr object)))
      object))


;;; More Commonly-used List Functions

(defun revappend (x y)
  "Return (append (reverse x) y)"
  (do ((top x (cdr top))
       (result y (cons (car top) result)))
      ((endp top) result)))

;;; NCONC finds the first non-null list, so it can make splice point to a cons.
;;; After finding the first cons element, it holds it in a result variable
;;; while running down successive elements tacking them together.  While
;;; tacking lists together, if we encounter a null list, we set the previous
;;; list's last cdr to nil just in case it wasn't already nil, and it could
;;; have been dotted while the null list was the last argument to NCONC.  The
;;; manipulation of splice (that is starting it out on a first cons, setting
;;; LAST of splice, and setting splice to ele) inherently handles (nconc x x),
;;; and it avoids running down the last argument to NCONC which allows the last
;;; argument to be circular.
;;;
(defun nconc (&rest lists)
  "Concatenates the lists given as arguments (by changing them)"
  (do ((top lists (cdr top)))
      ((null top) nil)
    (let ((top-of-top (car top)))
      (typecase top-of-top
	(cons
	 (let* ((result top-of-top)
		(splice result))
	   (do ((elements (cdr top) (cdr elements)))
	       ((endp elements))
	     (let ((ele (car elements)))
	       (typecase ele
		 (cons (rplacd (last splice) ele)
		       (setf splice ele))
		 (null (rplacd (last splice) nil))
		 (atom (if (cdr elements)
			   (error "Argument is not a list -- ~S." ele)
			   (rplacd (last splice) ele)))
		 (t (error "Argument is not a list -- ~S." ele)))))
	   (return result)))
	(null)
	(atom
	 (if (cdr top)
	     (error "Argument is not a list -- ~S." top-of-top)
	     (return top-of-top)))
	(t (error "Argument is not a list -- ~S." top-of-top))))))

(defun nreconc (x y)
  "Return (nconc (nreverse x) y)"
  (do ((1st (cdr x) (if (atom 1st) 1st (cdr 1st)))
       (2nd x 1st)		;2nd follows first down the list.
       (3rd y 2nd))		;3rd follows 2nd down the list.
      ((atom 2nd) 3rd)
    (rplacd 2nd 3rd)))


(defun butlast (list &optional (n 1))
  "Return a new list the same as List without the last N conses.
   List must not be circular."
  (declare (list list) (type index n))
  (unless (null list)
    (let ((length (do ((list list (cdr list))
		       (i 0 (1+ i)))
		      ((atom list) (1- i)))))
      (declare (type index length))
      (unless (< length n)
	(do* ((top (cdr list) (cdr top))
	      (result (list (car list)))
	      (splice result)
	      (count length (1- count)))
	     ((= count n) result)
	  (declare (type index count))
	  (setq splice (cdr (rplacd splice (list (car top))))))))))

(defun nbutlast (list &optional (n 1))
  "Modifies List to remove the last N conses. List must not be circular."
  (declare (list list) (type index n))
  (unless (null list)
    (let ((length (do ((list list (cdr list))
		       (i 0 (1+ i)))
		      ((atom list) (1- i)))))
      (declare (type index length))
      (unless (< length n)
	(do ((1st (cdr list) (cdr 1st))
	     (2nd list 1st)
	     (count length (1- count)))
	    ((= count n)
	     (rplacd 2nd ())
	     list)
	  (declare (type index count)))))))

(defun ldiff (list object)
  "Returns a new list, whose elements are those of List that appear before
   Object.  If Object is not a tail of List, a copy of List is returned.
   List must be a proper list or a dotted list."
  (do* ((list list (cdr list))
	(result (list ()))
	(splice result))
       ((atom list)
	(if (eql list object)
	    (cdr result)
	    (progn (rplacd splice list) (cdr result))))
    (if (eql list object)
	(return (cdr result))
	(setq splice (cdr (rplacd splice (list (car list))))))))


;;; Functions to alter list structure

(defun rplaca (x y)
  "Changes the car of x to y and returns the new x."
  (rplaca x y))

(defun rplacd (x y)
  "Changes the cdr of x to y and returns the new x."
  (rplacd x y))

;;; The following are for use by SETF.

(defun %rplaca (x val) (rplaca x val) val)

(defun %rplacd (x val) (rplacd x val) val)

(defun %setnth (n list newval)
  (declare (type index n))
  "Sets the Nth element of List (zero based) to Newval."
  (do ((count n (1- count))
       (list list (cdr list)))
      ((endp list)
       (error "~S is too large an index for SETF of NTH." n))
    (declare (fixnum count))
    (when (<= count 0)
      (rplaca list newval)
      (return newval))))


;;;; :key arg optimization to save funcall of IDENTITY.

;;; APPLY-KEY saves us a function call sometimes.
;;;    This is not in an (eval-when (compile eval) ...
;;;    because this is used in seq.lisp and sort.lisp.
;;;
(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

(defun identity (x)
  "Return the argument, $x."  ;; FIX multiple values?
  x)

(defun t (&rest args)
  "Return t."
  (declare (ignore args))
  t)

#|
((lambda ())) == (f)
((lambda (&rest)) ...) == (f ...)

(defun f (&rest args)
  "Return ()."
  (declare (ignore args))
  ())
|#

(defun complement (function)
  "Builds a new function that returns T whenever FUNCTION returns NIL and
   NIL whenever FUNCTION returns T."
  #'(lambda (&optional (arg0 nil arg0-p) (arg1 nil arg1-p) (arg2 nil arg2-p)
		       &rest more-args)
      (not (cond (more-args (apply function arg0 arg1 arg2 more-args))
		 (arg2-p (funcall function arg0 arg1 arg2))
		 (arg1-p (funcall function arg0 arg1))
		 (arg0-p (funcall function arg0))
		 (t (funcall function))))))

(defun constantly (value &optional (val1 nil val1-p) (val2 nil val2-p)
			 &rest more-values)
  "Build a function that always returns $value, and possibly $more-values."
  (cond (more-values
	 (let ((list (list* value val1 val2 more-values)))
	   #'(lambda ()
	       (declare (ext:optimize-interface (speed 3) (safety 0)))
	       (values-list list))))
	(val2-p
	 #'(lambda ()
	     (declare (ext:optimize-interface (speed 3) (safety 0)))
	     (values value val1 val2)))
	(val1-p
	 #'(lambda ()
	     (declare (ext:optimize-interface (speed 3) (safety 0)))
	     (values value val1)))
	(t
	 #'(lambda ()
	     (declare (ext:optimize-interface (speed 3) (safety 0)))
	     value))))


;;;; Macros for (&key (key #'identity) (test #'eql testp) (test-not nil notp)).
;;; Use these with the following keyword args:
;;;
(defmacro with-set-keys (funcall)
  `(cond ((and testp notp) (error "Test and test-not both supplied."))
	 (notp ,(append funcall '(:key key :test-not test-not)))
	 (t ,(append funcall '(:key key :test test)))))

(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
      (cond (testp (funcall test ,item ,key-tmp))
	    (notp (not (funcall test-not ,item ,key-tmp)))
	    (t (funcall test ,item ,key-tmp))))))

;;; Substitution of expressions

(defun subst (new old tree &key key (test #'eql testp) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (let ((car (s (car subtree)))
			     (cdr (s (cdr subtree))))
			 (if (and (eq car (car subtree))
				  (eq cdr (cdr subtree)))
			     subtree
			     (cons car cdr)))))))
    (s tree)))

(defun subst-if (new test tree &key key)
  "Substitutes new for subtrees for which test is true."
  (labels ((s (subtree)
	      (cond ((funcall test (apply-key key subtree)) new)
		    ((atom subtree) subtree)
		    (t (let ((car (s (car subtree)))
			     (cdr (s (cdr subtree))))
			 (if (and (eq car (car subtree))
				  (eq cdr (cdr subtree)))
			     subtree
			     (cons car cdr)))))))
    (s tree)))

(defun subst-if-not (new test tree &key key)
  "Substitutes new for subtrees for which test is false."
  (labels ((s (subtree)
	      (cond ((not (funcall test (apply-key key subtree))) new)
		    ((atom subtree) subtree)
		    (t (let ((car (s (car subtree)))
			     (cdr (s (cdr subtree))))
			 (if (and (eq car (car subtree))
				  (eq cdr (cdr subtree)))
			     subtree
			     (cons car cdr)))))))
    (s tree)))

(defun nsubst (new old tree &key key (test #'eql testp) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (satisfies-the-test old subtree)
				 (setf (cdr last) new)))
			 (if (satisfies-the-test old subtree)
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun nsubst-if (new test tree &key key)
  "Substitutes new for subtrees of tree for which test is true."
  (labels ((s (subtree)
	      (cond ((funcall test (apply-key key subtree)) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (funcall test (apply-key key subtree))
				 (setf (cdr last) new)))
			 (if (funcall test (apply-key key subtree))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun nsubst-if-not (new test tree &key key)
  "Substitutes new for subtrees of tree for which test is false."
  (labels ((s (subtree)
	      (cond ((not (funcall test (apply-key key subtree))) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (not (funcall test (apply-key key subtree)))
				 (setf (cdr last) new)))
			 (if (not (funcall test (apply-key key subtree)))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))


(defun sublis (alist tree &key key (test #'eql) (test-not nil notp))
  "Substitutes from alist into tree nondestructively."
  (declare (inline assoc))
  (labels ((s (subtree)
	     (let* ((key-val (apply-key key subtree))
		    (assoc (if notp
			       (assoc key-val alist :test-not test-not)
			       (assoc key-val alist :test test))))
	       (cond (assoc (cdr assoc))
		     ((atom subtree) subtree)
		     (t (let ((car (s (car subtree)))
			      (cdr (s (cdr subtree))))
			  (if (and (eq car (car subtreE))
				   (eq cdr (cdr subtree)))
			      subtree
			      (cons car cdr))))))))
    (s tree)))

;;; In run-time env, since can be referenced in line expansions.
(defmacro nsublis-macro ()
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key subtree)))
      (if notp
	  (assoc ,key-tmp alist :test-not test-not)
	  (assoc ,key-tmp alist :test test)))))

(defun nsublis (alist tree &key key (test #'eql) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  (declare (inline assoc))
  (let (temp)
    (labels ((s (subtree)
		(cond ((Setq temp (nsublis-macro))
		       (cdr temp))
		      ((atom subtree) subtree)
		      (t (do* ((last nil subtree)
			       (subtree subtree (Cdr subtree)))
			      ((atom subtree)
			       (if (setq temp (nsublis-macro))
				   (setf (cdr last) (cdr temp))))
			   (if (setq temp (nsublis-macro))
			       (return (setf (Cdr last) (Cdr temp)))
			       (setf (car subtree) (s (car subtree)))))
			 subtree))))
      (s tree))))


;;;; Functions for using lists as sets

(defun member (item list &key key (test #'eql testp) (test-not nil notp))
  "Returns tail of list beginning with first element satisfying EQLity,
   :test, or :test-not with a given item."
  (do ((list list (cdr list)))
      ((null list) nil)
    (let ((car (car list)))
      (if (satisfies-the-test item car)
	  (return list)))))

(defun member-if (test list &key key)
  "Returns tail of list beginning with first element satisfying test(element)"
  (do ((list list (Cdr list)))
      ((endp list) nil)
    (if (funcall test (apply-key key (car list)))
	(return list))))

(defun member-if-not (test list &key key)
  "Returns tail of list beginning with first element not satisfying test(el)"
  (do ((list list (cdr list)))
      ((endp list) ())
    (if (not (funcall test (apply-key key (car list))))
	(return list))))

(defun tailp (object list)
  "Returns true if Object is the same as some tail of List, otherwise
   returns false. List must be a proper list or a dotted list."
  (do ((list list (cdr list)))
      ((atom list) (eql list object))
    (if (eql object list)
	(return t))))

(defun adjoin (item list &key key (test #'eql) (test-not nil notp))
  "Add item to list unless it is already a member"
  (declare (inline member))
  (if (let ((key-val (apply-key key item)))
	(if notp
	    (member key-val list :test-not test-not :key key)
	    (member key-val list :test test :key key)))
      list
      (cons item list)))

;;; UNION -- Public.
;;;
;;; This function assumes list2 is the result, adding to it from list1 as
;;; necessary.  List2 must initialize the result value, so the call to MEMBER
;;; will apply the test to the elements from list1 and list2 in the correct
;;; order.
;;;
(defun union (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Returns the union of list1 and list2."
  (declare (inline member))
  (when (and testp notp) (error "Test and test-not both supplied."))
  (let ((res list2))
    (dolist (elt list1)
      (unless (with-set-keys (member (apply-key key elt) list2))
	(push elt res)))
    res))

;;; Destination and source are setf-able and many-evaluable.  Sets the source
;;; to the cdr, and "conses" the first elt of source to destination.
;;;
(defmacro steve-splice (source destination)  ; FIX steve?
  `(let ((temp ,source))
     (setf ,source (cdr ,source)
	   (cdr temp) ,destination
	   ,destination temp)))

(defun nunion (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Destructively returns the union list1 and list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res list2)
	(list1 list1))
    (do ()
	((endp list1))
      (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setf list1 (cdr list1))))
    res))

(defun intersection (list1 list2 &key key
			   (test #'eql testp) (test-not nil notp))
  "Return the intersection of list1 and list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil))
    (dolist (elt list1)
      (if (with-set-keys (member (apply-key key elt) list2))
	  (push elt res)))
    res))

(defun nintersection (list1 list2 &key key
			    (test #'eql testp) (test-not nil notp))
  "Destructively returns the intersection of list1 and list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (with-set-keys (member (apply-key key (car list1)) list2))
	  (steve-splice list1 res)
	  (setq list1 (Cdr list1))))
    res))

(defun set-difference (list1 list2 &key key
			     (test #'eql testp) (test-not nil notp))
  "Returns the elements of list1 which are not in list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (if (null list2)
      list1
      (let ((res nil))
	(dolist (elt list1)
	  (if (not (with-set-keys (member (apply-key key elt) list2)))
	      (push elt res)))
	res)))

(defun nset-difference (list1 list2 &key key
			      (test #'eql testp) (test-not nil notp))
  "Destructively returns the elements of list1 which are not in list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setq list1 (cdr list1))))
    res))

(defun set-exclusive-or (list1 list2 &key key
			       (test #'eql testp) (test-not nil notp))
  "Returns new list of elements appearing exactly once in list1 and list2."
  (declare (inline member))
  (let ((result nil))
    (dolist (elt list1)
      (unless (with-set-keys (member (apply-key key elt) list2))
	(setq result (cons elt result))))
    (dolist (elt list2)
      (unless (with-set-keys (member (apply-key key elt) list1))
	(setq result (cons elt result))))
    result))

;;; The outer loop examines list1 while the inner loop examines list2. If an
;;; element is found in list2 "equal" to the element in list1, both are
;;; spliced out. When the end of list1 is reached, what is left of list2 is
;;; tacked onto what is left of list1.  The splicing operation ensures that
;;; the correct operation is performed depending on whether splice is at the
;;; top of the list or not

(defun nset-exclusive-or (list1 list2 &key (test #'eql) (test-not nil notp)
				key)
  "Destructively return a list with elements which appear but once in list1
   and list2."
  (do ((list1 list1)
       (list2 list2)
       (x list1 (cdr x))
       (splicex ()))
      ((endp x)
       (if (null splicex)
	   (setq list1 list2)
	   (rplacd splicex list2))
       list1)
    (do ((y list2 (cdr y))
	 (splicey ()))
	((endp y) (setq splicex x))
      (cond ((let ((key-val-x (apply-key key (car x)))
		   (key-val-y (apply-key key (Car y))))
	       (if notp
		   (not (funcall test-not key-val-x key-val-y))
		   (funcall test key-val-x key-val-y)))
	     (if (null splicex)
		 (setq list1 (cdr x))
		 (rplacd splicex (cdr x)))
	     (if (null splicey)
		 (setq list2 (cdr y))
		 (rplacd splicey (cdr y)))
	     (return ()))			; assume lists are really sets
	    (t (setq splicey y))))))

(defun subsetp (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Returns T if every element in list1 is also in list2."
  (declare (inline member))
  (dolist (elt list1)
    (unless (with-set-keys (member (apply-key key elt) list2))
      (return-from subsetp nil)))
  T)


;;; Functions that operate on association lists

(defun acons (key datum alist)
  "Construct a new alist by adding the pair (key . datum) to alist"
  (cons (cons key datum) alist))

(defun pairlis (keys data &optional (alist '()))
  "Construct an association list from keys and data (adding to alist)"
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((and (endp x) (endp y)) alist)
    (if (or (endp x) (endp y))
	(error "The lists of keys and data must be of equal length."))
    (setq alist (acons (car x) (car y) alist))))

;;; In run-time environment, since these guys can be inline expanded.
(defmacro assoc-guts (test-guy)
  `(do ((alist alist (cdr alist)))
       ((endp alist))
     (if (car alist)
	 (if ,test-guy (return (car alist))))))

(defun assoc (item alist &key key test test-not)
  "Return the cons in $alist whose car is equal (by a given test or EQL) to
   $item."
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (caar alist))))
	     (assoc-guts (funcall test item (caar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (caar alist)))))
	     (assoc-guts (not (funcall test-not item (caar alist))))))
	(t
	 (if key
	     (assoc-guts (eql item (funcall key (caar alist))))
	     (assoc-guts (eql item (caar alist)))))))

;; FIX reconsider this
;;         to be consistent, every fun with a :test must have a fun=

(defun assoc= (item alist &key key test test-not)
  "Return the cons in $alist whose car is equal (by a given $test or EQUAL)
   to $item."
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (caar alist))))
	     (assoc-guts (funcall test item (caar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (caar alist)))))
	     (assoc-guts (not (funcall test-not item (caar alist))))))
	(t
	 (if key
	     (assoc-guts (equal item (funcall key (caar alist))))
	     (assoc-guts (equal item (caar alist)))))))

(defun assoc-if (predicate alist &key key)
  "Returns the first cons in alist whose car satisfies the Predicate.  If
   key is supplied, apply it to the car of each cons before testing."
  (if key
      (assoc-guts (funcall predicate (funcall key (caar alist))))
      (assoc-guts (funcall predicate (caar alist)))))

(defun assoc-if-not (predicate alist &key key)
  "Returns the first cons in alist whose car does not satisfiy the Predicate.
  If key is supplied, apply it to the car of each cons before testing."
  (if key
      (assoc-guts (not (funcall predicate (funcall key (caar alist)))))
      (assoc-guts (not (funcall predicate (caar alist))))))

(defun rassoc (item alist &key key test test-not)
  "Return the cons in $alist whose cdr is equal (by a given test or EQL) to
   $item."
  (declare (list alist))
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (cdar alist))))
	     (assoc-guts (funcall test item (cdar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (cdar alist)))))
	     (assoc-guts (not (funcall test-not item (cdar alist))))))
	(t
	 (if key
	     (assoc-guts (eql item (funcall key (cdar alist))))
	     (assoc-guts (eql item (cdar alist)))))))

(defun rassoc= (item alist &key key test test-not)
  "Return the cons in $alist whose cdr is equal (by a given test or EQUAL)
   to $item."
  (declare (list alist))
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (cdar alist))))
	     (assoc-guts (funcall test item (cdar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (cdar alist)))))
	     (assoc-guts (not (funcall test-not item (cdar alist))))))
	(t
	 (if key
	     (assoc-guts (equal item (funcall key (cdar alist))))
	     (assoc-guts (equal item (cdar alist)))))))

(defun rassoc-if (predicate alist &key key)
  "Returns the first cons in alist whose cdr satisfies the Predicate.  If key
  is supplied, apply it to the cdr of each cons before testing."
  (if key
      (assoc-guts (funcall predicate (funcall key (cdar alist))))
      (assoc-guts (funcall predicate (cdar alist)))))

(defun rassoc-if-not (predicate alist &key key)
  "Returns the first cons in alist whose cdr does not satisfy the Predicate.
  If key is supplied, apply it to the cdr of each cons before testing."
  (if key
      (assoc-guts (not (funcall predicate (funcall key (cdar alist)))))
      (assoc-guts (not (funcall predicate (cdar alist))))))


;;;; Mapping functions.

(defun map1 (function original-arglists accumulate take-car)
  "This function is called by mapc, mapcar, mapcan, mapl, maplist, and
   mapcon.  It Maps function over the arglists in the appropriate way. It
   is done when any of the arglists runs out.  Until then, it CDRs down the
   arglists calling the function and accumulating results as desired."
  (let* ((arglists (copy-list original-arglists))
	 (ret-list (list nil))
	 (temp ret-list))
    (do ((res nil)
	 (args '() '()))
	((dolist (x arglists nil) (if (null x) (return t)))
	 (if accumulate
	     (cdr ret-list)
	     (car original-arglists)))
      (do ((l arglists (cdr l)))
	  ((null l))
	(push (if take-car (caar l) (car l)) args)
	(setf (car l) (cdar l)))
      (setq res (apply function (nreverse args)))
      (case accumulate
	(:nconc (setq temp (last (nconc temp res))))
	(:list (rplacd temp (list res))
	       (setq temp (cdr temp)))))))

(defun mapc (function list &rest more-lists)
  "Applies fn to successive elements of lists, returns its second argument."
  (map1 function (cons list more-lists) nil t))

(defun mapcar (function list &rest more-lists)
  "Applies fn to successive elements of list, returns list of results."
  (map1 function (cons list more-lists) :list t))

(defun mapcan (function list &rest more-lists)
  "Applies fn to successive elements of list, returns NCONC of results."
  (map1 function (cons list more-lists) :nconc t))

(defun mapl (function list &rest more-lists)
  "Applies fn to successive CDRs of list, returns ()."
  (map1 function (cons list more-lists) nil nil))

(defun maplist (function list &rest more-lists)
  "Applies fn to successive CDRs of list, returns list of results."
  (map1 function (cons list more-lists) :list nil))

(defun mapcon (function list &rest more-lists)
  "Applies fn to successive CDRs of lists, returns NCONC of results."
  (map1 function (cons list more-lists) :nconc nil))

;; FIX reconsider  (reduce #'concat '("a" "b"))
(defun mapconcat (function list &optional (spacer "") spacer-first)
  "Return the concatenation of the results of applying FUNCTION to
   succussive CARs of LIST.  Insert SPACER between each result, and if
   SPACER-FIRST add a leading spacer."
  (if list
      (concatenate 'simple-string
		   (if spacer-first
		       (concatenate 'simple-string
				    spacer
				    (funcall function (car list)))
		       (funcall function (car list)))
		   (mapconcat function (cdr list) spacer t))
      ""))


;;; Functions for compatibility sake:
;;; FIX always use others (member, assoc, delete)?
;;;     similar to assoc=

(defun memq (item list)
  "Returns tail of list beginning with first element eq to item"
  (declare (inline member))
  (member item list :test #'eq))

(defun assq (item alist)
  "Return the first pair of alist where item EQ the key of pair"
  (declare (inline assoc))
  (assoc item alist :test #'eq))

(defun delq (item list)
  "Returns list with all elements EQ to ITEM deleted."
  (let ((list list))
    (do ((x list (cdr x))
	 (splice '()))
	((endp x) list)
      (cond ((eq item (car x))
	     (if (null splice)
		 (setq list (cdr x))
		 (rplacd splice (cdr x))))
	    (T (setq splice x))))))	; move splice along to include element


;; FIX is there some function that does this already?

(defun duplicatesp (list &key (test #'equal))
  "Return true if list contains duplicate entries, comparing elements with
   function $test."
  (iterate iter ((one list))
    (when one
      ;; FIX use `member' instd of looping?
      (let ((one-element (car one)))
	(dolist (two (cdr one))
	  (if (funcall test one-element two)
	      (return-from duplicatesp t))))
      (iter (cdr one)))))
