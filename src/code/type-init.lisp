;;; Precompute the translations for commonly used type specifiers.  This is
;;; Split off from the other type code to get around problems with
;;; everything needing to be loaded before everything else.  This is the
;;; first file which really exercises the type stuff.  This stuff is also
;;; somewhat implementation-dependent in that implementations may want to
;;; precompute other types which are important to them.

(in-package "KERNEL")

(export '*null-type*)

;;;; Built-in classes.

(dolist (x built-in-classes)
  (destructuring-bind (name &key (translation nil trans-p) &allow-other-keys)
		      x
    (when trans-p
      (let ((class (class-cell-class (find-class-cell name)))
	    (type (specifier-type translation)))
	(setf (built-in-class-translation class) type)
	(setf (info type builtin name) type)))))

;;;; Numeric types.

(precompute-types '((mod 2) (mod 4) (mod 16) (mod #x100) (mod #x10000)
		    (mod #x100000000)
		    (unsigned-byte 1) (unsigned-byte 2) (unsigned-byte 4)
		    (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
		    (signed-byte 8) (signed-byte 16) (signed-byte 32)))

;;;; Builtin symbol type specifiers.

(precompute-types type-specifier-symbols)

(defparameter *null-type* (specifier-type 'null))
