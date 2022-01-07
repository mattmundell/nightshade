;;; Hexadecimal editing mode.

;; TODO Support extending the buffer.

;; FIX (setf (ed::buffer-major-mode (ed::current-buffer)) "Fundamental")
;;          when change out must revert buffer to deep-region

(in-package "ED")

#[ Hex Mode

FIX autodetect

{mode:Hex}
{command:Hex}
]#

;; Return the hex character associated with $number.
;;
(defun hex-from-number (number)
  (code-char
   (cond ((< number 10)
	  (+ (char-code #\0) number))
	 (t
	  (+ (char-code #\A) (- number 10))))))
;;
(declaim (inline hex-from-number))

;; Return the number associated with hex $char.
;;
(defun hex-to-number (char)
  (let ((code (char-code char)))
    (cond ((and (>= code (char-code #\0))
		(<= code (char-code #\9)))
	   (- code (char-code #\0)))
	  ((and (>= code (char-code #\a))
		(<= code (char-code #\f)))
	   (+ (- code (char-code #\a)) 10))
	  ((and (>= code (char-code #\A))
		(<= code (char-code #\F)))
	   (+ (- code (char-code #\A)) 10)))))
;;
(declaim (inline hex-to-number))

;; Refresh hex $buffer starting input from $mark.
;;
(defun refresh-hex (buffer mark)
  (let ((point (buffer-point buffer))
	(mark2 (copy-mark mark))
	(chars (make-string 9))
	(address 0))
    (setf (char chars 8) #\newline)
    (delete-region (buffer-region buffer))
    (block buffer-loop
      (loop
	(if (zerop (rem address 32))
	    (insert-string point (format () "~8<~D~>  " address))
	    (insert-string point "          "))
	(while ((byte 0 (1+ byte)))
	       ((< byte 8))
	  (let* ((char (or (next-character mark2)
			   (progn
			     (dotimes (i (- 8 byte))
			       (insert-string point
					      (if (eq i 0)
						  ".." " .."))
			       (if (eq i 3)
				   (insert-character point #\space)))
			     (if (plusp byte)
				 (insert-string point chars 0 byte))
			     (insert-character point #\newline)
			     (return-from buffer-loop))))
		 (code (char-code char)))
	    (mark-after mark2)
	    (setf (aref chars byte) (if (graphic-char-p char)
					char #\.))
	    (insert-character point
			      (hex-from-number (ash code -4)))
	    (insert-character point
			      (hex-from-number (logand #b1111 code)))
	    (if (eq byte 3)
		(insert-character point #\space))
	    #| for byte going to 16
	    (if (eq (rem byte 8) 7)
		(insert-character point #\space))
	    |#
	    (insert-character point #\space)))
	(insert-string point chars)
	(incf address 8)))
    (buffer-start point)
    (setf (buffer-modified buffer) ())))

(defun setup-hex-buffer (buffer)
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
      (defevar "Hex Start"
	"Start mark of the text in the deep buffer."
	:buffer buffer
	:value start)
      (defevar "Hex Previous Mode"
	"The mode that was major in the buffer before Hex mode."
	:buffer buffer
	;; TODO Maybe add an arg passed to `setup-hex-buffer'.
	:value "Fundamental") ;(buffer-major-mode buffer))
      (defevar "Add End Newline On Writing File"
	"If true check for a trailing newline when writing the file."
	:buffer buffer)
      (defevar "Flush Trailing Whitespace"
	"If true flush trailing whitespace when writing the file."
	:buffer buffer)
      (refresh-hex buffer start))
    (highlight-visible-hex-buffer buffer)
    (pushnew '("Hex" t highlight-visible-hex-buffer)
	     *mode-highlighters*)))

(defmode "Hex" :major-p t :setup-function #'setup-hex-buffer
  :documentation "A mode for viewing and editing binary files as ~
		  hexidecimal.")

(defcommand "Hex" ()
  "Toggle Hex Mode"
  (let ((buffer (current-buffer)))
    (if (buffer-modified buffer)
	(editor-error "Please save file first."))
    (if (string= (buffer-major-mode buffer) "Hex")
	(progn
	  (setf (buffer-major-mode buffer)
		(value hex-previous-mode))
	  (setf (buffer-deep-region buffer) ())
	  (defevar "Hex Off"
	    "Flag for switching out of Hex Mode."
	    :buffer buffer
	    :value t)
	  (revert-file-command))
	(progn
	  (defevar "Hex Off"
	    "Flag for switching out of Hex Mode."
	    :buffer buffer
	    :value ())
	  (hex-mode-command)))))

(defcommand "Hex Refresh" ()
  "Refresh Hex buffer."
  (or (string= (buffer-major-mode (current-buffer)) "Hex")
      (error "Buffer must be in Hex mode."))
  (if (prompt-for-y-or-n :prompt "Refresh buffer? ")
      (refresh-hex (current-buffer) (value hex-start))))

(defvar *start-of-ascii* 35)

(defcommand "Hex g or Refresh" ()
  "Either scroll window down or insert a space depending on point
   position."
  (or (string= (buffer-major-mode (current-buffer)) "Hex")
      (error "Buffer must be in Hex mode."))
  (let ((column (mark-column (current-point))))
    (if (and (>= column *start-of-ascii*)
	     (< column (+ *start-of-ascii* 8)))
	(set-ascii-character (current-point) #\g (value hex-start))
	(hex-refresh-command))))

;;; Return the charpos of the hex pair associated with the ASCII character
;;; at index $pos (relative to the start of the 8 ASCII characters).
;;;
(defun hex-pair-position (pos)
  (let ((hex (+ 10 (* 3 pos))))
    (if (> pos 3) (1+ hex) hex)))

;;; Return the charpos of the ASCII character associated with the hex digit
;;; at position $pos.
;;;
(defun hex-ascii-position (pos)
  (+ (truncate (- pos (if (> pos 21) 11 10)) 3) *start-of-ascii*))

;; Return the two-character string that represents $char in hex.
;;
(defun hex-pair-string (char)
  (let ((code (char-code char)))
    (format () "~C~C"
	    (hex-from-number (ash code -4))
	    (hex-from-number (logand #b1111 code)))))

;; Set the ASCII character at $mark to $char.
;;
(defun set-ascii-character (mark char deep-start)
  (let ((next (next-character mark)))
    (if (if next (char= next #\newline) t)
	(editor-error "FIX implement extending the buffer"))
    (let ((mark2 (copy-mark mark)))
      (setf (mark-charpos mark2)
	    (hex-pair-position (- (mark-column mark)
				  *start-of-ascii*)))
      (let ((code (char-code char)))
	(setf (next-character mark2) (hex-from-number (ash code -4)))
	(mark-after mark2)
	(setf (next-character mark2) (hex-from-number (logand #b1111 code))))
      (delete-mark mark2))
    #|
    (flet ((insert-end-deep (deep-start char)
	     (let ((deep-mark (copy-mark deep-start)))
	       (buffer-end deep-mark)
	       (insert-character deep-mark char)
	       (delete-mark deep-mark)))))
    |#
    ;; Set the corresponding deep character.
    (let ((deep-mark (copy-mark deep-start))
	  (start (buffer-start-mark (mark-buffer mark))))
      (character-offset deep-mark
			(+ (* (1- (count-lines (region start mark))) 8)
			   (- (mark-charpos mark) *start-of-ascii*)))
      (setf (next-character deep-mark) char)
      (delete-mark deep-mark))
    ;; Set the buffer character.
    (setf (next-character mark) char)
    (mark-after mark)))

(defcommand "Hex Space or Scroll Window Down" ()
  "Either scroll window down or insert a space, depending on position of
   point."
  (or (string= (buffer-major-mode (current-buffer)) "Hex")
      (error "Buffer must be in Hex mode."))
  (let ((column (mark-column (current-point))))
    (if (and (>= column *start-of-ascii*)
	     (< column (+ *start-of-ascii* 8)))
	(set-ascii-character (current-point) #\space (value hex-start))
	(scroll-window-down-command))))

(defcommand "Hex Next Item" ()
  "Move to the next editable item."
  (or (string= (buffer-major-mode (current-buffer)) "Hex")
      (error "Buffer must be in Hex mode."))
  (scroll-window-down-command))

;;; Return true if $column is one of a hex pair.
;;;
(defun hex-column-p (column)
  (and (> column 9)
       (< column (1- *start-of-ascii*))
       (cond ((< column 21)
	      (plusp (rem (+ column 3) 3)))
	     ((> column 22)
	      (plusp (rem (+ column 2) 3))))))

;; Set the hex character at $mark to $char.  Also set the corresponding
;; character in the deep buffer, which starts at mark $deep-start.
;;
(defun set-hex-character (mark char deep-start)
  (let ((next (next-character mark)))
    (if (if next (char= next #\.) t)
	(editor-error "FIX implement extending the buffer")))
  (let ((mark2 (copy-mark mark)))
    (let* ((left-p (char= (previous-character mark2) #\space))
	   (left (if left-p
		     char
		     (previous-character mark2)))
	   (right (if left-p
		      (progn
			(mark-after mark2)
			(next-character mark2))
		      char))
	   (char (code-char (logior (ash (hex-to-number left) 4)
				    (hex-to-number right)))))
      (setf (mark-charpos mark2)
	    (hex-ascii-position (mark-column mark)))
      (setf (next-character mark2)
	    (if (graphic-char-p char) char #\.))
      ;; Set the corresponding deep character.
      (let ((deep-mark (copy-mark deep-start))
	    (start (buffer-start-mark (mark-buffer mark2))))
	(character-offset deep-mark
			  (+ (* (1- (count-lines (region start mark2))) 8)
			     (- (mark-charpos mark2) *start-of-ascii*)))
	(setf (next-character deep-mark) char)
	(delete-mark deep-mark)))
    (delete-mark mark2))
#|
  (flet ((insert-end-deep (deep-start char)
	   (let ((deep-mark (copy-mark deep-start)))
	     (buffer-end deep-mark)
	     (insert-character deep-mark char)
	     (delete-mark deep-mark)))))
|#
  (setf (next-character mark) char)
  (mark-after mark))

(defcommand "Self Insert Hex" ()
  "Insert the last typed character if possible."
  (or (string= (buffer-major-mode (current-buffer)) "Hex")
      (error "Buffer must be in Hex mode."))
  (let ((column (mark-column (current-point))))
    (cond ((and (>= column *start-of-ascii*)
		(< column (+ *start-of-ascii* 8)))
	   (let ((char (ext:key-event-char *last-key-event-typed*)))
	     (or char (editor-error "Failed to insert last character."))
	     (set-ascii-character (current-point) char
				  (value hex-start))))
	  ((hex-column-p column)
	   (let ((char (ext:key-event-char *last-key-event-typed*)))
	     (or char (editor-error "Failed to insert last character."))
	     (let ((code (char-code char)))
	       (if (or (and (>= code (char-code #\0))
			    (<= code (char-code #\9)))
		       (and (>= code (char-code #\a))
			    (<= code (char-code #\f)))
		       (and (>= code (char-code #\A))
			    (<= code (char-code #\F))))
		   (set-hex-character (current-point)
				      (char-upcase char)
				      (value hex-start))
		   (editor-error "Hex columns must be hex characters: 0-9 a-f A-F")))))
	  (t
	   (editor-error "That part of the buffer is read only.")))))

(defcommand "Hex q or Quit" ()
  "Either rotate the buffers or insert a space, depending on position of
   point."
  (or (string= (buffer-major-mode (current-buffer)) "Hex")
      (error "Buffer must be in Hex mode."))
  (let ((column (mark-column (current-point))))
    (if (and (>= column *start-of-ascii*)
	     (< column (+ *start-of-ascii* 8)))
	(set-ascii-character (current-point) #\space (value hex-start))
	(rotate-buffers-forward-command))))


;;;; Highlighting.

(defun highlight-hex-line (line chi-info)
  (let ((line-length (line-length line)))
    (when (>= line-length (1- *start-of-ascii*))
      (chi-mark line 0 *original-font* :variable chi-info)
      (chi-mark line 10 *original-font* :window-foreground chi-info)
      (if (>= line-length *start-of-ascii*)
	  (chi-mark line *start-of-ascii*
		    *original-font* :comment
		    chi-info)))))

(defun highlight-visible-hex-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-hex-line))


;;;; File opening check.

(defevar "Hex Check Length"
  "The number of characters the check for binary data when opening a file."
  :value 200)

(define-file-content-hook "Hex" (buffer)
  (or (and (editor-bound-p 'hex-off :buffer buffer)
	   (variable-value 'hex-off :buffer buffer))
      (while* ((mark (copy-mark (buffer-start-mark buffer))
		     (mark-after mark))
	       (count 0 (1+ count))
	       (hex-check-length (value hex-check-length)))
	      ((and mark (< count hex-check-length)))
	(let ((char (next-character mark)))
	  (or char (return))
	  (or (standard-char-p char)
	      (char= char #\tab)
	      (char= char #\page)
	      (char= char #\newline)
	      (char= char #\return)
	      (progn
		(message "Character ~D is ~D, using Hex Mode."
			 count (char-code char))
		(setf (buffer-major-mode buffer) "Hex")
		(return t)))))))

#|
(setq *file-content-hooks*
      (delete "Hex" *file-content-hooks* :key #'car :test #'string=))
|#
