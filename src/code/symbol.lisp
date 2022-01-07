;;; Symbol manipulating functions.
;;;
;;; Many of these are trivial interpreter entries to functions open-coded
;;; by the compiler.

(in-package "LISP")

(export '(get remprop symbol-plist getf get-properties symbol-name
	  make-symbol copy-symbol gensym gentemp *gensym-counter*
	  symbol-package keywordp makunbound symbol-value symbol-function
	  boundp set))

(in-package "KERNEL")
(export '(%set-symbol-value %set-symbol-plist %set-symbol-package fset))

(in-package "LISP")


#[ Symbols

All symbols and package names are printed in lower case, as a user is
likely to type them.  Internally, they are normally stored upper case only.
]#


(declaim (maybe-inline get %put getf remprop %putf get-properties keywordp))

(defun symbol-value (variable)
  "VARIABLE must evaluate to a symbol.  This symbol's current special value
   is returned."
  (declare (optimize (safety 1)))
  (symbol-value variable))

(defun boundp (variable)
  "VARIABLE must evaluate to a symbol.  Return NIL if this symbol is
   unbound, T if it has a value."
  (boundp variable))

(defun set (variable new-value)
  "VARIABLE must evaluate to a symbol.  This symbol's special value cell is
   set to the specified new value."
  (declare (type symbol variable))
  (cond ((null variable)
	 ;; FIX Should it be possible to get here?
	 (error "Attempt to set NIL."))
	((eq variable t)
	 ;; FIX So t is constant? Why? (Might want to temporarily bind over it.)
	 ;;     rather have #t and #f
	 (error "Attempt to set T."))
	((and (boundp '*keyword-package*)
	      (keywordp variable))
	 (error "Attempt to set a keyword."))
	(t
	 (%set-symbol-value variable new-value))))

(defun %set-symbol-value (symbol new-value)
  (%set-symbol-value symbol new-value))

(defun makunbound (variable)
  "VARIABLE must evaluate to a symbol.  This symbol is made unbound,
   removing any value it may currently have."
  (set variable
       (%primitive make-other-immediate-type 0 vm:unbound-marker-type))
  variable)

(defun symbol-function (variable)
  "VARIABLE must evaluate to a symbol.  This symbol's current definition is
   returned.  Settable with SETF."
  (raw-definition variable))

(defun fset (symbol new-value)
  (declare (type symbol symbol) (type (or function null) new-value))
  (setf (raw-definition symbol) new-value))

(defun symbol-plist (variable)
  "VARIABLE must evaluate to a symbol.  Return its property list."
  (symbol-plist variable))

(defun %set-symbol-plist (symbol new-value)
  (setf (symbol-plist symbol) new-value))

(defun symbol-name (variable)
  "VARIABLE must evaluate to a symbol.  Return its print name."
  (symbol-name variable))

(defun symbol-package (variable)
  "VARIABLE must evaluate to a symbol.  Return its package."
  (symbol-package variable))

(defun %set-symbol-package (symbol package)
  (declare (type symbol symbol))
  (%set-symbol-package symbol package))

(defun make-symbol (string)
  "Make and return a new symbol with the STRING as its print name."
  #-gengc (make-symbol string)
  #+gengc (%make-symbol (random most-positive-fixnum) string))

#+(or gengc x86)
(defun symbol-hash (symbol)
  "Return the hash value for symbol."
  (symbol-hash symbol))

(defun get (symbol indicator &optional (default nil))
  "Look on the property list of SYMBOL for the specified INDICATOR.  If
   this is found, return the associated value, else return DEFAULT."
  (do ((pl (symbol-plist symbol) (cddr pl)))
      ((atom pl) default)
    (cond ((atom (cdr pl))
	   (error "~S has an odd number of items in its property list."
		   symbol))
	  ((eq (car pl) indicator)
	   (return (cadr pl))))))

(defun %put (symbol indicator value)
  "The VALUE is added as a property of SYMBOL under the specified
   INDICATOR.  Returns VALUE."
  (do ((pl (symbol-plist symbol) (cddr pl)))
      ((endp pl)
       (setf (symbol-plist symbol)
	     (list* indicator value (symbol-plist symbol)))
       value)
    (cond ((endp (cdr pl))
	   (error "~S has an odd number of items in its property list."
		  symbol))
	  ((eq (car pl) indicator)
	   (rplaca (cdr pl) value)
	   (return value)))))

(defun remprop (symbol indicator)
  "Look on property list of SYMBOL for property with specified
   INDICATOR.  If found, splice this indicator and its value out of
   the plist, and return the tail of the original list starting with
   INDICATOR.  If not found, return () with no side effects.

   NOTE: The ANSI specification requires REMPROP to return true (not false)
   or false (the symbol NIL). Portable code should not rely on any other value."
  (do ((pl (symbol-plist symbol) (cddr pl))
       (prev nil pl))
      ((atom pl) nil)
    (cond ((atom (cdr pl))
	   (error "~S has an odd number of items in its property list."
		  symbol))
	  ((eq (car pl) indicator)
	   (cond (prev (rplacd (cdr prev) (cddr pl)))
		 (t
		  (setf (symbol-plist symbol) (cddr pl))))
	   (return pl)))))

(defun getf (place indicator &optional (default ()))
  "Searches the property list stored in Place for an indicator EQ to
   Indicator.  If one is found, the corresponding value is returned, else
   the Default is returned."
  (do ((plist place (cddr plist)))
      ((null plist) default)
    (cond ((atom (cdr plist))
	   (error "~S is a malformed property list."
		  place))
	  ((eq (car plist) indicator)
	   (return (cadr plist))))))

(defun %putf (place property new-value)
  (declare (type list place))
  (do ((plist place (cddr plist)))
      ((endp plist) (list* property new-value place))
    (declare (type list plist))
    (when (eq (car plist) property)
      (setf (cadr plist) new-value)
      (return place))))


(defun get-properties (place indicator-list)
  "Like GETF, except that Indicator-List is a list of indicators which will
   be looked for in the property list stored in Place.  Three values are
   returned, see manual for details."
  (do ((plist place (cddr plist)))
      ((null plist) (values nil nil nil))
    (cond ((atom (cdr plist))
	   (error "~S is a malformed proprty list."
		  place))
	  ((memq (car plist) indicator-list)
	   (return (values (car plist) (cadr plist) plist))))))

(defun copy-symbol (symbol &optional (copy-props nil) &aux new-symbol)
  "Make and return a new uninterned symbol with the same print name as
   SYMBOL.  If COPY-PROPS is false, the new symbol is neither bound nor
   fbound and has no properties, else it has a copy of SYMBOL's function,
   value and property list."
  (declare (type symbol symbol))
  (setq new-symbol (make-symbol (symbol-name symbol)))
  (when copy-props
    (%set-symbol-value new-symbol (%primitive fast-symbol-value symbol))
    (setf (symbol-plist new-symbol) (copy-list (symbol-plist symbol)))
    (when (fboundp symbol)
      (setf (symbol-function new-symbol) (symbol-function symbol))))
  new-symbol)

(proclaim '(special *keyword-package*))

(defun keywordp (object)
  "Returns true if Object is a symbol in the keyword package."
  (and (symbolp object)
       (eq (symbol-package object) *keyword-package*)))


;;;; Gensym and friends.

(defvar *gensym-counter* 0
  "Counter for generating unique GENSYM symbols.")
(declaim (type unsigned-byte *gensym-counter*))

(defun gensym (&optional (thing "G"))  ; FIX what is thing?
  "Creates a new uninterned symbol whose name is a prefix string (defaults
   to \"G\"), followed by a decimal number.  Thing, when supplied, will
   alter the prefix if it is a string, or be used for the decimal number if
   it is a number, of this symbol. The default value of the number is the
   current value of *gensym-counter* which is incremented each time it is
   used."
  (let ((old *gensym-counter*))
    (unless (numberp thing)
      (let ((new (etypecase old
		   (index (1+ old))
		   (unsigned-byte (1+ old)))))
	(declare (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
	(setq *gensym-counter* new)))
    (multiple-value-bind
	(prefix int)
	(etypecase thing
	  (simple-string (values thing old))
	  (fixnum (values "G" thing))
	  (string (values (coerce thing 'simple-string) old)))
      (declare (simple-string prefix))
      (make-symbol
       (concatenate 'simple-string prefix
		    (the simple-string
			 (quick-integer-to-string int)))))))

(defvar *gentemp-counter* 0)
(declaim (type index *gentemp-counter*))

(defun gentemp (&optional (prefix t) (package *package*))
  "Creates a new symbol interned in package Package with the given Prefix."
  (loop
    (let ((*print-base* 10)
	  (*print-radix* nil)
	  (*print-pretty* nil)
	  (new-pname (format nil "~A~D"
			     (string prefix) (incf *gentemp-counter*))))
      (multiple-value-bind (symbol existsp)
			   (find-symbol new-pname package)
	(declare (ignore symbol))
	(or existsp (return (values (intern new-pname package))))))))
