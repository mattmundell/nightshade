;;; Character set commands.

(in-package "ED")


;;;; Setting buffer charset.

(defun set-buffer-charset (buffer charset)
  (let ((name (edi::charset-name charset))
	(buffer-charset (if (editor-bound-p 'charset :buffer buffer)
			    (variable-value 'charset :buffer buffer))))
    (fi* (eq buffer-charset name)
      (or (eq name :utf-8)
	  (or (and buffer-charset
		   (or (eq buffer-charset :utf-8)
		       ;; FIX convert to utf-8 then to new charset, with
		       ;; ~warning
		       (editor-error "Set buffer to UTF-8 first")))
	      (let ((modified (buffer-modified buffer)))
		(filter-region (edi::charset-to-utf-8-fun charset)
			       (buffer-region buffer))
		(or modified (setf (buffer-modified buffer) ()))
		(buffer-start (buffer-point buffer)))))
      (defevar "Charset"
	"The character set of the text in the file shown in the buffer.  The
	 text in the buffer is always in the internal character set."
	:value name
	:buffer buffer))))

(defcommand "Set Buffer Charset" ()
  "Set the character set of the file in the current buffer."
  (set-buffer-charset
   (current-buffer)
   (nth-value 1 (prompt-for-keyword (list edi::*charsets-table*)
				    :prompt "Charset: "))))


;;;; UTF-8 hacks.

(defcommand "Input UTF8 Umlaut A" (p &optional (mark (current-point)))
  "Input a UTF-8 a with an umlaut."
  (declare (ignore p))
  (insert-character mark (code-char 195))
  (insert-character mark (code-char 164))
  t)

(defcommand "Input UTF8 Umlaut A Caps" (p &optional (mark (current-point)))
  "Input a UTF-8 A with an umlaut."
  (declare (ignore p))
  (insert-character mark (code-char 195))
  (insert-character mark (code-char 132))
  t)

(defcommand "Input UTF8 Umlaut O" (p &optional (mark (current-point)))
  "Input a UTF-8 o with an umlaut."
  (declare (ignore p))
  (insert-character mark (code-char 195))
  (insert-character mark (code-char 182))
  t)

(defcommand "Input UTF8 Umlaut O Caps" (p &optional (mark (current-point)))
  "Input a UTF-8 O with an umlaut."
  (declare (ignore p))
  (insert-character mark (code-char 195))
  (insert-character mark (code-char 150))
  t)

(defcommand "Input UTF8 Umlaut U" (p &optional (mark (current-point)))
  "Input a UTF-8 u with an umlaut."
  (declare (ignore p))
  (insert-character mark (code-char 195))
  (insert-character mark (code-char 188))
  t)

(defcommand "Input UTF8 Umlaut U Caps" (p &optional (mark (current-point)))
  "Input a UTF-8 U with an umlaut."
  (declare (ignore p))
  (insert-character mark (code-char 195))
  (insert-character mark (code-char 156))
  t)

(defcommand "Input UTF8 SS" (p &optional (mark (current-point)))
  "Input a UTF-8 sharp s."
  (declare (ignore p))
  (insert-character mark (code-char 195))
  (insert-character mark (code-char 159))
  t)

(defcommand "Input Pound" (p &optional (mark (current-point)))
  "Input a pound sign."
  (declare (ignore p))
  (insert-character mark (code-char 194))
  (insert-character mark (code-char 163))
  t)
