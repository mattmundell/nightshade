;;; Definitions of various types of streams used in the editor.

(in-package "EDI")

(export '(make-editor-output-stream
	  editor-region-stream editor-region-stream-p
	  editor-output-stream make-editor-region-stream
	  editor-output-stream-p make-kbdmac-stream
	  modify-kbdmac-stream))

#[ Editor Streams

It is possible to create streams which output to or get input from a buffer.
This mechanism is quite powerful and permits easy interfacing of the editor to
Lisp.

{function:ed:make-editor-output-stream}
{function:ed:editor-output-stream-p}
{function:ed:make-editor-region-stream}
{function:ed:editor-region-stream-p}
{function:ed:with-input-from-region}
{function:ed:with-output-to-mark}
{function:ed:with-pop-up-display}
]#

(defstruct (editor-output-stream
	    (:include sys:lisp-stream
		      (:misc #'editor-output-misc))
	    (:print-function %print-editor-output-stream)
	    (:constructor internal-make-editor-output-stream ()))
  "FIX"
  ;;
  ;; The mark at which the stream inserts.
  mark)

(setf (documentation 'editor-output-stream-p 'function)
  "Return t if x is an editor-output-stream.")

(defun %print-editor-output-stream (s stream d)
  (declare (ignore d s))
  (write-string "#<Editor output stream>" stream))

(defun make-editor-output-stream (mark &optional (buffered :line))
  "Return a stream that inserts at the permanent mark $mark all output
   directed to it.  $Buffered controls whether the stream is buffered.  Its
   valid values are:

     :none
	Write characters to the screen immediately.

     :line
	The buffer is flushed whenever a newline is written or when it is
	explicitly done with `force-output'.

     :full
	The screen is only brought up to date when it is explicitly done
	with `force-output'."
  (modify-editor-output-stream (internal-make-editor-output-stream)
			       mark
			       buffered))

(defun modify-editor-output-stream (stream mark buffered)
  (or (and (markp mark)
	   (memq (mark-kind mark) '(:right-inserting :left-inserting)))
      (error "~S is a temporary mark." mark))
  (setf (editor-output-stream-mark stream) mark)
  (case buffered
    (:none
     (setf (lisp::lisp-stream-out stream) #'editor-output-unbuffered-out
	   (lisp::lisp-stream-sout stream) #'editor-output-unbuffered-sout))
    (:line
     (setf (lisp::lisp-stream-out stream) #'editor-output-line-buffered-out
	   (lisp::lisp-stream-sout stream) #'editor-output-line-buffered-sout))
    (:full
     (setf (lisp::lisp-stream-out stream) #'editor-output-buffered-out
	   (lisp::lisp-stream-sout stream) #'editor-output-buffered-sout))
    (t
     (error "~S is a losing value for Buffered." buffered)))
  stream)

(defmacro with-left-inserting-mark ((var form) &body forms)
  (let ((change (gensym)))
    `(let* ((,var ,form)
	    (,change (eq (mark-kind ,var) :right-inserting)))
       (unwind-protect
	   (progn
	     (when ,change
	       (setf (mark-kind ,var) :left-inserting))
	     ,@forms)
	 (when ,change
	   (setf (mark-kind ,var) :right-inserting))))))

(defun editor-output-unbuffered-out (stream character)
  (with-left-inserting-mark (mark (editor-output-stream-mark stream))
    (insert-character mark character)
    (redisplay-windows-from-mark mark)))

(defun editor-output-unbuffered-sout (stream string start end)
  (with-left-inserting-mark (mark (editor-output-stream-mark stream))
    (insert-string mark string start end)
    (redisplay-windows-from-mark mark)))

(defun editor-output-buffered-out (stream character)
  (with-left-inserting-mark (mark (editor-output-stream-mark stream))
    (insert-character mark character)))

(defun editor-output-buffered-sout (stream string start end)
  (with-left-inserting-mark (mark (editor-output-stream-mark stream))
    (insert-string mark string start end)))

(defun editor-output-line-buffered-out (stream character)
  (with-left-inserting-mark (mark (editor-output-stream-mark stream))
    (insert-character mark character)
    (when (char= character #\newline)
      (redisplay-windows-from-mark mark))))

(defun editor-output-line-buffered-sout (stream string start end)
  (declare (simple-string string))
  (with-left-inserting-mark (mark (editor-output-stream-mark stream))
    (insert-string mark string start end)
    (when (find #\newline string :start start :end end)
      (redisplay-windows-from-mark mark))))

(defun editor-output-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:charpos (mark-charpos (editor-output-stream-mark stream)))
    (:line-length
     (let* ((buffer (line-buffer (mark-line (editor-output-stream-mark stream)))))
       (when buffer
	 (do ((w (buffer-windows buffer) (cdr w))
	      (min most-positive-fixnum (min (window-width (car w)) min)))
	     ((null w)
	      (if (/= min most-positive-fixnum) min))))))
    (:file-position
     (let ((mark (editor-output-stream-mark stream)))
       (count-characters (region (buffer-start-mark
				  (line-buffer (mark-line mark)))
				 mark))))
    ((:finish-output :force-output)
     (redisplay-windows-from-mark (editor-output-stream-mark stream)))
    (:close (setf (editor-output-stream-mark stream) nil))
    (:element-type 'base-char)))


(defstruct (editor-region-stream
	    (:include sys:lisp-stream
		      (:in #'region-in)
		      (:misc #'region-misc))
	    (:print-function %print-region-stream)
	    (:constructor internal-make-editor-region-stream (region mark)))
  "FIX"
  ;;
  ;; The region read.
  region
  ;;
  ;; The mark pointing to the next character to read.
  mark)

(setf (documentation 'editor-region-stream-p 'function)
  "Return t if x is an editor-region-stream.")

(defun %print-region-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<Editor region stream>" stream))

(defun make-editor-region-stream (region)
  "Return an input stream from which the text in $region can be read."
  (internal-make-editor-region-stream
   region (copy-mark (region-start region) :right-inserting)))

(defun modify-editor-region-stream (stream region)
  (setf (editor-region-stream-region stream) region)
  (let* ((mark (editor-region-stream-mark stream))
	 (start (region-start region))
	 (start-line (mark-line start)))
    ;; Make sure it's dead.
    (delete-mark mark)
    (setf (mark-line mark) start-line  (mark-charpos mark) (mark-charpos start))
    (push mark (line-marks start-line)))
  stream)

(defun region-in (stream eof-errorp eof-value)
  (let ((mark (editor-region-stream-mark stream)))
    (cond ((mark< mark
		  (region-end (editor-region-stream-region stream)))
	   (prog1 (next-character mark) (mark-after mark)))
	  (eof-errorp (error "~A hit end of file." stream))
	  (t eof-value))))

(defun region-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:listen (mark< (editor-region-stream-mark stream)
		    (region-end (editor-region-stream-region stream))))
    (:clear-input (move-mark
                   (editor-region-stream-mark stream)
                   (region-end (editor-region-stream-region stream))))
    (:unread
     (let ((mark (editor-region-stream-mark stream)))
       (unless (mark> mark
		      (region-start (editor-region-stream-region stream)))
	 (error "Nothing to unread."))
       (unless (char= arg1 (previous-character mark))
	 (error "Unreading something not read: ~S" arg1))
       (mark-before mark)))
    (:file-position
     (let ((start (region-start (editor-region-stream-region stream)))
	   (mark (editor-region-stream-mark stream)))
       (cond (arg1
	      (move-mark mark start)
	      (character-offset mark arg1))
	     (t
	      (count-characters (region start mark))))))
    (:close
     (delete-mark (editor-region-stream-mark stream))
     (setf (editor-region-stream-region stream) nil))
    (:element-type 'base-char)))


;;;; Stuff to support keyboard macros.

(defstruct (kbdmac-stream
	    (:include editor-input
		      (:get #'kbdmac-get)
		      (:unget #'kbdmac-unget)
		      (:listen #'kbdmac-listen))
	    (:constructor make-kbdmac-stream ()))
  buffer    ; The simple-vector that holds the characters.
  index)    ; Index of the next character.

(defun kbdmac-get (stream ignore-abort-attempts-p)
  (declare (ignore ignore-abort-attempts-p))
  (let ((index (kbdmac-stream-index stream)))
    (setf (kbdmac-stream-index stream) (1+ index))
    (setq *last-key-event-typed*
	  (svref (kbdmac-stream-buffer stream) index))))

(defun kbdmac-unget (ignore stream)
  (declare (ignore ignore))
  (if (plusp (kbdmac-stream-index stream))
      (decf (kbdmac-stream-index stream))
      (error "Nothing to unread.")))

(defun kbdmac-listen (stream)
  (declare (ignore stream))
  t)

;;; MODIFY-KBDMAC-STREAM  --  Internal
;;;
;;; Bash the kbdmac-stream Stream so that it will return the Input.
;;;
(defun modify-kbdmac-stream (stream input)
  (setf (kbdmac-stream-index stream) 0)
  (setf (kbdmac-stream-buffer stream) input)
  stream)
