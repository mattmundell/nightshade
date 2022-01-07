;;; Registers for holding text and positions.

(in-package "ED")


#[ Registers

Registers allow you to save a text position or chunk of text associated with a
key-event.  This is a convenient way to repeatedly access a commonly-used
location or text fragment.

{command:Save Position}
{command:Jump to Saved Position}
{command:Put Register}
{command:Get Register}
{command:List Registers}
{command:Kill Register}
]#


;;;; Registers implementation.

;;; Registers are named by characters.  Each register refers to a mark or
;;; a cons of a region and the buffer it came from.
;;;
(defvar *registers* (make-hash-table))

(defun register-count ()
  (hash-table-count *registers*))

(defun register-value (reg-name)
  (gethash reg-name *registers*))

(defsetf register-value (reg-name) (new-value)
  (let ((name (gensym))
	(value (gensym))
	(old-value (gensym)))
    `(let* ((,name ,reg-name)
	    (,value ,new-value)
	    (,old-value (gethash ,name *registers*)))
       (when (and ,old-value (markp ,old-value))
	 (delete-mark ,old-value))
       (setf (gethash ,name *registers*) ,value))))

(defun prompt-for-register (&optional (prompt "Register: ") must-exist)
  (let ((reg-name (prompt-for-key-event :prompt prompt)))
    (unless (or (not must-exist) (gethash reg-name *registers*))
      (editor-error "Register ~A is empty." reg-name))
    reg-name))

(defmacro do-registers ((name value &optional sorted) &rest body)
  (if sorted
      (let ((sorted-regs (gensym))
	    (reg (gensym)))
	`(let ((,sorted-regs nil))
	   (declare (list ,sorted-regs))
	   (maphash #'(lambda (,name ,value)
			(push (cons ,name ,value) ,sorted-regs))
		    *registers*)
	   (setf ,sorted-regs (sort ,sorted-regs #'char-lessp :key #'car))
	   (dolist (,reg ,sorted-regs)
	     (let ((,name (car ,reg))
		   (,value (cdr ,reg)))
	       ,@body))))
      `(maphash #'(lambda (,name ,value)
		    ,@body)
		*registers*)))

;;; Hook to clean things up if a buffer is deleted while registers point to it.
;;;
(defun flush-reg-references-to-deleted-buffer (buffer)
  (do-registers (name value)
    (etypecase value
      (mark (when (eq (line-buffer (mark-line value)) buffer)
	      (free-register name)))
      (cons (free-register-value value buffer)))))
;;;
(add-hook delete-buffer-hook 'flush-reg-references-to-deleted-buffer)

(defun free-register (name)
  (let ((value (register-value name)))
    (when value (free-register-value value)))
  (remhash name *registers*))

(defun free-register-value (value &optional buffer)
  (etypecase value
    (mark
     (when (or (not buffer) (eq (line-buffer (mark-line value)) buffer))
       (delete-mark value)))
    (cons
     (when (and buffer (eq (cdr value) buffer))
       (setf (cdr value) nil)))))


;;;; Commands.

;;; These commands all stash marks and regions with marks that point into some
;;; buffer, and they assume that the register values have the same property.
;;;

(defcommand "Save Position" ()
  "Save the current textual position in a register."
  (let ((reg-name (prompt-for-register)))
    (setf (register-value reg-name)
	  (copy-mark (current-point) :left-inserting))))

(defcommand "Jump to Saved Position" ()
  "Move the point to a location previously saved in a register, including
   changing to the buffer in which the location was defined."
  (let* ((reg-name (prompt-for-register "Jump to Register: " t))
	 (val (register-value reg-name)))
    (unless (markp val)
      (editor-error "Register ~A does not hold a location." reg-name))
    (change-to-buffer (line-buffer (mark-line val)))
    (move-mark (current-point) val)))

(defcommand "Kill Register" ()
  "Kill a register."
  (free-register (prompt-for-register "Register to kill: ")))

(defcommand "List Registers" ()
  "Pop up a list of all registers along with a description of the contents
   of each."
  (with-pop-up-display (f :height (* 2 (register-count)))
    (do-registers (name val :sorted)
      (write-string "Reg " f)
      (ext:print-pretty-key-event name f)
      (write-string ":  " f)
      (etypecase val
	(mark
	 (let* ((line (mark-line val))
		(buff (line-buffer line))
		(len (line-length line)))
	   (format f "Line ~S, col ~S in buffer ~A~%   ~A~:[~;...~]~%"
		   (count-lines (region (buffer-start-mark buff) val))
		   (mark-column val)
		   (buffer-name buff)
		   (subseq (line-string line) 0 (min 61 len))
		   (> len 60))))
	(cons
	 (let* ((str (region-to-string (car val)))
		(nl (position #\newline str :test #'char=))
		(len (length str))
		(buff (cdr val)))
	   (declare (simple-string str))
	   (format f "Text~@[ from buffer ~A~]~%   ~A~:[~;...~]~%"
		   (if buff (buffer-name buff))
		   (subseq str 0 (if nl (min 61 len nl) (min 61 len)))
		   (> len 60))))))))

(defcommand "Put Register" ()
  "Copy the current region into a register."
  (let ((region (current-region)))
    ;; Bind the region before prompting in case the region isn't active.
    (setf (register-value (prompt-for-register))
	  (cons (copy-region region) (current-buffer)))))

(defcommand "Get Register" ()
  "Insert at point the region from a register."
  (let* ((reg-name (prompt-for-register "Register from which to get text: " t))
	 (val (register-value reg-name)))
    (or (and (consp val) (regionp (car val)))
	(editor-error "Register ~A does not hold a region." reg-name))
    (let ((point (current-point)))
      (push-buffer-mark (copy-mark point))
      (insert-region (current-point) (car val))))
  (setf (last-command-type) :ephemerally-active))
