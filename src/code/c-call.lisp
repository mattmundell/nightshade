;;; Some extensions to the Alien facility to simplify importing C
;;; interfaces.

(in-package "C-CALL")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")
(use-package "SYSTEM")

(export '(char short int long unsigned-char unsigned-short unsigned-int
	  unsigned-long float double c-string void))


#[ The C-Call Package

The c-call package exports these type-equivalents to the C type of the
same name: char, short, int, long,
unsigned-char, unsigned-short, unsigned-int,
unsigned-long, float, double.  c-call also exports
these types:

{alien-type:void}
{alien-type:c-string}
]#


;;;; Extra types.

(def-alien-type char (integer 8))
(def-alien-type short (integer 16))
(def-alien-type int (integer 32))
(def-alien-type long (integer #-alpha 32 #+alpha 64))

(def-alien-type unsigned-char (unsigned 8))
(def-alien-type unsigned-short (unsigned 16))
(def-alien-type unsigned-int (unsigned 32))
(def-alien-type unsigned-long (unsigned #-alpha 32 #+alpha 64))

(def-alien-type float single-float)
(def-alien-type double double-float)

(def-alien-type-translator void ()
  "This type is used in function types to declare that an arbitrary value
   is returned.  Evaluation of an alien-funcall form will return zero
   values."
  (parse-alien-type '(values)))



;;;; C string support.

(def-alien-type-class (c-string :include pointer :include-args (to))
  "This type is similar to (* char), but is interpreted as a
   null-terminated string, and is automatically converted into a Lisp
   string when accessed.  If the pointer is C NULL (or 0), then accessing
   gives Lisp ().

   Assigning a Lisp string to a c-string structure field or variable stores
   the contents of the string to the memory already pointed to by that
   variable.  When an Alien of type (* char) is assigned to a c-string,
   then the c-string pointer is assigned to.  This allows c-string pointers
   to be initialized.  For example:

       (def-alien-type nil (struct foo (str c-string)))

       (defun make-foo (str)
	 (let ((my-foo (make-alien (struct foo))))
	   (setf (slot my-foo 'str) (make-alien char (length str)))
	   (setf (slot my-foo 'str) str)
	   my-foo))

   Storing Lisp () writes C NULL to the c-string pointer.")

(def-alien-type-translator c-string ()
  (make-alien-c-string-type :to (parse-alien-type 'char)))

(def-alien-type-method (c-string :unparse) (type)
  (declare (ignore type))
  'c-string)

(def-alien-type-method (c-string :lisp-rep) (type)
  (declare (ignore type))
  '(or simple-base-string null (alien (* char))))

(def-alien-type-method (c-string :naturalize-gen) (type alien)
  (declare (ignore type))
  `(if (zerop (sap-int ,alien))
       nil
       (%naturalize-c-string ,alien)))

(def-alien-type-method (c-string :deport-gen) (type value)
  (declare (ignore type))
  `(etypecase ,value
     (null (int-sap 0))
     ((alien (* char)) (alien-sap ,value))
     (simple-base-string (vector-sap ,value))))

(defun %naturalize-c-string (sap)
  (declare (type system-area-pointer sap))
  (with-alien ((ptr (* char) sap))
    (locally
     (declare (optimize (speed 3) (safety 0)))
     (let ((length (loop
		     for offset of-type fixnum upfrom 0
		     until (zerop (deref ptr offset))
		     finally (return offset))))
       (let ((result (make-string length)))
	 (kernel:copy-from-system-area (alien-sap ptr) 0
				       result (* vm:vector-data-offset
						 vm:word-bits)
				       (* length vm:byte-bits))
	 result)))))
