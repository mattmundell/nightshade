;;; -*- Package: ED -*-
;;;
;;; FASL file reading mode.

(defpackage "FASL"
  (:version 3)
  (:use "LISP" "EXT")
  (:documentation "FASL editor mode."))

;; TODO Add editing.

;; FIX (setf (ed::buffer-major-mode (ed::current-buffer)) "Fundamental")
;;          when change out must revert buffer to deep-region

(in-package "ED")

;; FIX how to install this to the dir? rather access via package docs
#[ FASL Mode

Autodetected according to file extensions: .fasl .assem

{mode:FASL}
{command:FASL}

[ Fasload File Format ]  Description of the FASL format.
]#


;;;; Reading from the FASL buffer.

(defun read-integer (mark n)
  (until ((res (char-code (next-character mark))
	       (logior (ash (char-code (next-character mark)) 8)
		       res))
	  (cnt 1 (1+ cnt)))
	 ((= cnt n)
	  (mark-after mark)
	  res)
    (mark-after mark)))

(defun read-string (mark n)
  (while ((string (make-string n))
	  (cnt 0 (1+ cnt)))
	 ((< cnt n) string)
    (setf (aref string cnt) (next-character mark))
    (mark-after mark)))

(defmacro read-signed-integer (mark size)
  (if (integerp size)
      (let ((n-last (gensym)))
	(until ((res `(let ((,n-last (char-code (next-character ,mark))))
			(mark-after ,mark)
			(if (zerop (logand ,n-last #x80))
			    ,n-last
			    (logior ,n-last #x-100)))
		     `(prog1
			  (logior (char-code (next-character ,mark))
				  (ash (the (signed-byte ,(* cnt 8)) ,res) 8))
			(mark-after ,mark)))
		(cnt 1 (1+ cnt)))
	       ((>= cnt size) res)))
      `(read-sized-signed-integer ,mark ,size)))

(defun read-sized-signed-integer (mark size)
  (eval `(read-signed-integer ,mark ,size)))


;;;; FOP argument printers.

(defvar fop-printers (make-array 256 :initial-element ())
  "Vector indexed by a FaslOP that yields the FOP's name.")

(defmacro define-fop-printer ((&rest fops) &body body)
  (let ((fun (gensym)) (fop (gensym)))
    `(progn
       (defun ,fun (mark deep-mark)
	 ,@body)
       (dolist (,fop ',fops)
	 (setf (svref ,fop-printers (get (intern (symbol-name ,fop) "LISP") 'lisp::fop-code))
	       #',fun)))))

; fop-nop
; fop-pop

(define-fop-printer (fop-push fop-alter-code fop-function-entry)
  (let ((index (read-integer deep-mark 4)))
    (insert-string mark
		   (format () " index(~D)" index))))

(define-fop-printer (fop-byte-push)
  (insert-string mark
		 (format () " ~2,'0X"
			 (next-character deep-mark)))
  (mark-after deep-mark))

; fop-empty-list
; fop-truth

(define-fop-printer (fop-symbol-save fop-uninterned-symbol-save
		     fop-lisp-symbol-save
		     fop-keyword-symbol-save)
  (let* ((size (read-integer deep-mark 4))
	 (name (read-string deep-mark size)))
    (insert-string mark
		   (format () " n(~D) name(~S)" size name))))

(define-fop-printer (fop-small-symbol-save
		     fop-uninterned-small-symbol-save
		     fop-lisp-small-symbol-save
		     fop-keyword-small-symbol-save)
  (let* ((size (read-integer deep-mark 1))
	 (name (read-string deep-mark size)))
    (insert-string mark
		   (format () " n(~D) name(~S)" size name))))

(define-fop-printer (fop-symbol-in-package-save)
  (let* ((index (read-integer deep-mark 4))
	 (size (read-integer deep-mark 4))
	 (name (read-string deep-mark size)))
    (insert-string mark
		   (format () " index(~D) n(~D) name(~S)"
			   index size name))))

(define-fop-printer (fop-small-symbol-in-package-save)
  (let* ((index (read-integer deep-mark 4))
	 (size (read-integer deep-mark 1))
	 (name (read-string deep-mark size)))
    (insert-string mark
		   (format () " index(~D) n(~D) name(~S)"
			   index size name))))

(define-fop-printer (fop-symbol-in-byte-package-save)
  (let* ((index (read-integer deep-mark 1))
	 (size (read-integer deep-mark 4))
	 (name (read-string deep-mark size)))
    (insert-string mark
		   (format () " index(~D) n(~D) name(~S)"
			   index size name))))

(define-fop-printer (fop-small-symbol-in-byte-package-save)
  (let* ((index (read-integer deep-mark 1))
	 (size (read-integer deep-mark 1))
	 (name (read-string deep-mark size)))
    (insert-string mark
		   (format () " index(~D) n(~D) name(~S)"
			   index size name))))

; fop-package

(define-fop-printer (fop-list fop-list*)
  (let ((length (read-integer deep-mark 1)))
    (insert-string mark
		   (format () " length(~D)" length))))

; fop-list-1
; fop-list-2
; fop-list-3
; fop-list-4
; fop-list-5
; fop-list-6
; fop-list-7
; fop-list-8
; fop-list*-1
; fop-list*-2
; fop-list*-3
; fop-list*-4
; fop-list*-5
; fop-list*-6
; fop-list*-7
; fop-list*-8

(define-fop-printer (fop-integer)
  (let* ((size (read-integer deep-mark 4))
	 (int (read-signed-integer deep-mark size)))
    (insert-string mark
		   (format () " size(~D) value(~D)"
			   size int))))

(define-fop-printer (fop-small-integer)
  (let* ((size (read-integer deep-mark 4))
	 (int (read-signed-integer deep-mark size)))
    (insert-string mark
		   (format () " size(~D) value(~D)"
			   size int))))

(define-fop-printer (fop-small-integer)
  (let* ((size (read-integer deep-mark 1))
	 (int (read-signed-integer deep-mark size)))
    (insert-string mark
		   (format () " size(~D) value(~D)"
			   size int))))

(define-fop-printer (fop-word-integer)
  (let ((int (read-signed-integer deep-mark 4)))
    (insert-string mark
		   (format () " value(~D)" int))))

(define-fop-printer (fop-byte-integer)
  (let ((int (read-signed-integer deep-mark 1)))
    (insert-string mark
		   (format () " value(~D)" int))))

(define-fop-printer (fop-string)
  (let* ((size (read-integer deep-mark 4))
	 (string (read-string deep-mark size)))
    (insert-string mark
		   (format () " n(~D) string(~S)"
			   size string))))

(define-fop-printer (fop-small-string)
  (let* ((size (read-integer deep-mark 1))
	 (string (read-string deep-mark size)))
    (insert-string mark
		   (format () " n(~D) string(~S)"
			   size string))))

(define-fop-printer (fop-vector fop-uniform-vector fop-struct)
  (let ((length (read-integer deep-mark 4)))
    (insert-string mark
		   (format () " n(~D)" length))))

(define-fop-printer (fop-small-vector fop-small-uniform-vector
                     fop-small-struct)
  (let ((length (read-integer deep-mark 1)))
    (insert-string mark
		   (format () " n(~D)" length))))

(define-fop-printer (fop-int-vector)
  (let ((length (read-integer deep-mark 4))
	(size (read-integer deep-mark 1)))
    (insert-string mark
		   (format () " n(~D) size(~D) data("
			   length size))
    (dotimes (count (* length (truncate size 8)))
      (insert-string mark
		     (format () "~@[ ~]~2,'0X"
			     (plusp count)
			     (next-character deep-mark)))
      (mark-after deep-mark))
    (insert-character mark #\))))

(define-fop-printer (fop-uniform-int-vector)
  (let* ((length (read-integer deep-mark 4))
	 (size (read-integer deep-mark 1))
	 (value (read-integer deep-mark (truncate size 8))))
    (insert-string mark
		   (format () " n(~D) size(~D) value(~D)"
			   length size value))))

(define-fop-printer (fop-single-float)
  (let ((int (read-signed-integer deep-mark 4)))
    (insert-string mark
		   (format () " data(~D)" int))))

(define-fop-printer (fop-double-float)
  (let ((int (read-signed-integer deep-mark 8)))
    (insert-string mark
		   (format () " data(~D)" int))))

; fop-eval
; fop-eval-for-effect

(define-fop-printer (fop-funcall)
  (let ((int (read-integer deep-mark 1)))
    (insert-string mark
		   (format () " nargs(~D)" int))))

(define-fop-printer (fop-funcall-for-effect)
  (let ((int (read-integer deep-mark 1)))
    (insert-string mark
		   (format () " nargs(~D)" int))))

(define-fop-printer (fop-code-format)
  (let ((implementation (read-integer deep-mark 1))
	(version (read-integer deep-mark 1)))
    (insert-string mark
		   (format () " implementation(~D) version(~D)"
			   implementation version))))

(define-fop-printer (fop-code)
  (let* ((nitems (read-integer deep-mark 4))
	 (size (read-integer deep-mark 4)))
    (insert-string mark
		   (format () " nitems(~D) size(~D) code("
			   nitems size))
    (dotimes (count size)
      (insert-string mark
		     (format () "~A~2,'0X"
			     (if (plusp count) " " "")
			     (next-character deep-mark)))
      (mark-after deep-mark))
    (insert-character mark #\))))

(define-fop-printer (fop-small-code)
  (let* ((nitems (read-integer deep-mark 1))
	 (size (read-integer deep-mark 2)))
    (insert-string mark
		   (format () " nitems(~D) size(~D) code("
			   nitems size))
    (dotimes (count size)
      (insert-string mark
		     (format () "~A~2,'0X"
			     (if (plusp count) " " "")
			     (char-code (next-character deep-mark))))
      (mark-after deep-mark))
    (insert-character mark #\))))

(define-fop-printer (fop-verify-table-size)
  (let ((size (read-integer deep-mark 4)))
    (insert-string mark (format () " size(~D)" size))))

; fop-verify-empty-stack
; fop-end-group
; fop-pop-for-effect
; fop-misc-trap

(define-fop-printer (fop-character)
  (dotimes (count 3)
    (insert-string mark
		   (format () "~@[ ~]~2,'0X"
			   (plusp count)
			   (next-character deep-mark)))
    (mark-after deep-mark)))

(define-fop-printer (fop-short-character)
  (insert-character mark #\space)
  (insert-character mark (next-character deep-mark))
  (mark-after deep-mark))

; fop-ratio
; fop-complex
; fop-fset
; fop-normal-load
; fop-maybe-cold-load

(define-fop-printer (fop-array)
  (let ((rank (read-integer deep-mark 4)))
    (insert-string mark
		   (format () " rank(~D)" rank))))

(define-fop-printer (fop-byte-alter-code)
  (let ((index (read-integer deep-mark 1)))
    (insert-string mark
		   (format () " index(~D)" index))))

(define-fop-printer (fop-assembler-code)
  (let ((length (read-integer deep-mark 4)))
    (insert-string mark
		   (format () " length(~D) code(" length))
    (dotimes (count length)
      (insert-string mark
		     (format () "~A~2,'0X"
			     (if (plusp count) " " "")
			     (char-code
			      (next-character deep-mark))))
      (mark-after deep-mark))
    (insert-character mark #\))))

(define-fop-printer (fop-assembler-routine
		     fop-assembler-fixup)
  (let ((length (read-integer deep-mark 4)))
    (insert-string mark
		   (format () " offset(~D)" length))))

(define-fop-printer (fop-foreign-fixup)
  (let* ((len (read-integer deep-mark 1))
	 (name (read-string deep-mark len))
	 (offset (read-string deep-mark 4)))
    (insert-string mark
		   (format () " len(~D) name(~S) index(~D)"
			   len name offset))))

(define-fop-printer (fop-rplaca fop-rplacd)
  (let* ((table-idx (read-integer deep-mark 4))
	 (cdr-offset (read-integer deep-mark 4)))
    (insert-string mark
		   (format () " table-idx(~D) cdr-offset(~D)"
			   table-idx cdr-offset))))

(define-fop-printer (fop-svset fop-structset)
  (let* ((table-idx (read-integer deep-mark 4))
	 (vector-idx (read-integer deep-mark 4)))
    (insert-string mark
		   (format () " table-idx(~D) vector-idx(~D)"
			   table-idx vector-idx))))

(define-fop-printer (fop-nthcdr)
  (let* ((cdr-offset (read-integer deep-mark 4)))
    (insert-string mark
		   (format () " cdr-offset(~D)" cdr-offset))))

; fop-end-header


;;;; Commands.

;; Refresh FASL $buffer starting input from $mark.
;;
(defun refresh-fasl (buffer mark)
  (with-writable-buffer (buffer)
    (let ((point (buffer-point buffer))
	  (mark2 (copy-mark mark)))
      (delete-region (buffer-region buffer))
      ;; Write the header.
      (while ((mark2 mark2 (mark-after mark2)))
	     (mark2)
	(let ((char (next-character mark2)))
	  (or char (return))
	  (if (char= char (code-char #xFF))
	      (progn
		;; Read over any #xFF's.
		(while ((mark2 (mark-after mark2) (mark-after mark2)))
		       ((and mark2
			     (next-character mark2)
			     (char= (next-character mark2)
				    (code-char #xFF)))
			(insert-character point #\newline)
			(insert-string point "FF (start marker)")
			(insert-character point #\newline)))
		(return)))
	  (insert-character point char)))
      ;; Write the ops.
      (while ((mark2 mark2))
	     (mark2)
	(let ((char (next-character mark2)))
	  (or char (return))
	  (insert-string point
			 (format () "~2,'0X ~30A"
				 (char-code char)
				 (let ((fop (svref lisp::fop-codes
						   (char-code char))))
				   (if (symbolp fop)
				       (symbol-name fop)
				       "(available FOP)"))))
	  (mark-after mark2)
	  (let ((printer (svref fop-printers (char-code char))))
	    (if printer (funcall printer point mark2)))
	  (insert-character point #\newline)))
      (elet ((flush-trailing-whitespace t))
	(flush-trailing-whitespace buffer))
      (buffer-start point))))

(defun setup-fasl-buffer (buffer)
  (let ((region (copy-region (buffer-region buffer))))
#|
    ;;; Caution.  Make the deep region look like it is a region of buffer.
    ;;; This is to allow modification of the deep with the likes of
    ;;; `insert-character'.  This may cause problems.
    (while ((line (mark-line (region-start region))
		  (line-next line)))
	   (line)
      (setf (edi::line-%buffer line) buffer))
|#

    (let ((start (copy-mark (region-start region))))
      (setf (buffer-deep-region buffer) region)
      (defevar "FASL Start"
	"Start mark of the text in the deep buffer."
	:buffer buffer
	:value start)
      (defevar "FASL Previous Mode"
	"The mode that was major in the buffer before FASL mode."
	:buffer buffer
	;; TODO Maybe add an arg passed to `setup-hex-buffer'.
	:value "Fundamental") ;(buffer-major-mode buffer))
      (defevar "Add End Newline On Writing File"
	"If true check for a trailing newline when writing the file."
	:buffer buffer)
      (defevar "Flush Trailing Whitespace"
	"If true flush trailing whitespace when writing the file."
	:buffer buffer)
      (refresh-fasl buffer start))
    (highlight-visible-fasl-buffer buffer)
    (pushnew '("FASL" t highlight-visible-fasl-buffer)
	     *mode-highlighters*)))

(defmode "FASL" :major-p t :setup-function #'setup-fasl-buffer
  :documentation "A mode for viewing FASL files.")

(defcommand "FASL" ()
  "Toggle FASL Mode"
  (let ((buffer (current-buffer)))
    (if (buffer-modified buffer)
	(editor-error "Please save file first."))
    (if (string= (buffer-major-mode buffer) "FASL")
	(progn
	  (setf (buffer-major-mode buffer)
		(value fasl-previous-mode))
	  (defevar "FASL Off"
	    "Flag for switching out of FASL Mode."
	    :buffer buffer
	    :value t)
	  (revert-file-command))
	(progn
	  (defevar "FASL Off"
	    "Flag for switching out of FASL Mode."
	    :buffer buffer
	    :value ())
	  (fasl-mode-command)))))

(defcommand "FASL Refresh" ()
  "Refresh FASL buffer."
  (or (string= (buffer-major-mode (current-buffer)) "FASL")
      (error "Buffer must be in FASL mode."))
  (if (prompt-for-y-or-n :prompt "Refresh buffer? ")
      (refresh-fasl (current-buffer) (value fasl-start))))


;;;; Highlighting.

(defun highlight-fasl-line (line chi-info)
  (let ((line-length (line-length line)))
    (if (plusp line-length)
	(or (digit-char-p (next-character (mark line 0)))
	    (chi-mark line 0 *original-font* :comment chi-info)))))

(defun highlight-visible-fasl-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-fasl-line))


;;;; File open hook.

(define-file-type-hook ("fasl" "assem") (buffer type)
  (declare (ignore type))
  (fi (if (editor-bound-p 'fasl-off :buffer buffer)
	  (value fasl-off))
      (setf (buffer-major-mode buffer) "FASL")))


;;;; Bindings.

(bind-key "FASL Refresh" #k"g" :mode "FASL")
(bind-key "Scroll Window Down" #k"space" :mode "FASL")
(bind-key "Scroll Window Up" #k"backspace" :mode "FASL")
(bind-key "Scroll Window Up" #k"delete" :mode "FASL")
(bind-key "Rotate Buffers Forward" #k"q" :mode "FASL")
