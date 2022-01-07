(in-package "EDI")

(defun %sp-byte-blt (src start dest dstart end)
  (%primitive byte-blt src start dest dstart end))

(defun lisp::sap-to-fixnum (x) (sap-int x))
(defun lisp::fixnum-to-sap (x) (int-sap x))
(defun lisp::%sp-make-fixnum (x) (%primitive make-fixnum x))
(defun lisp::fast-char-upcase (x) (char-upcase x))

