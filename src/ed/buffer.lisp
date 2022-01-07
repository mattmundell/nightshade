;;; Functions for changing modes and buffers.

(in-package "EDI")

(export '(*buffer-list*
	  buffer-modified buffer-region buffer-name buffer-pathname
	  buffer-major-mode buffer-minor-mode buffer-modeline-fields
	  buffer-modeline-field-p
	  current-buffer current-point current-line current-directory
	  in-recursive-edit exit-recursive-edit abort-recursive-edit
	  recursive-edit defmode mode-major-p mode-variables mode-documentation
	  unique-buffer-name make-unique-buffer
	  make-buffer delete-buffer copy-buffer with-writable-buffer
	  buffer-start-mark buffer-end-mark recursive-edit))



;;;; Some buffer structure support.

(defun buffer-writable (buffer)
  "Returns whether buffer may be modified."
  (buffer-%writable buffer))

(defun %set-buffer-writable (buffer value)
  (prog1 (setf (buffer-%writable buffer) value)
    (invoke-hook ed::buffer-writable-hook buffer value)))

;;; BUFFER-MODIFIED uses the buffer modification tick which is for redisplay.
;;; We can never set this down to "unmodify" a buffer, so we keep an
;;; unmodification tick.  The buffer is modified only if this is less than the
;;; modification tick.
;;;
(defun buffer-modified (buffer)
  "Return T if Buffer has been modified, NIL otherwise.  Can be set with Setf."
  (or (bufferp buffer) (error "~S is not a buffer." buffer))
  (> (buffer-modified-tick buffer) (buffer-unmodified-tick buffer)))

(defun %set-buffer-modified (buffer sense)
  "If true make the buffer modified, if NIL unmodified."
  (or (bufferp buffer) (error "~S is not a buffer." buffer))
  (invoke-hook ed::buffer-modified-hook buffer sense)
  (if sense
      (setf (buffer-modified-tick buffer) (tick))
      (setf (buffer-unmodified-tick buffer) (tick)))
  sense)


(proclaim '(inline buffer-name buffer-pathname buffer-region))

(defun buffer-region (buffer)
  "Return the region which contains Buffer's text."
  (buffer-%region buffer))

(defun %set-buffer-region (buffer new-region)
  (let ((old (buffer-region buffer)))
    (delete-region old)
    (ninsert-region (region-start old) new-region)
    old))

(defun buffer-name (buffer)
  "Return Buffer's string name."
  (buffer-%name buffer))

(proclaim '(special *buffer-names*))

(defun %set-buffer-name (buffer name)
  (multiple-value-bind (entry foundp) (getstring name *buffer-names*)
    (cond ((or (not foundp) (eq entry buffer))
	   (invoke-hook ed::buffer-name-hook buffer name)
	   (delete-string (buffer-%name buffer) *buffer-names*)
	   (setf (getstring name *buffer-names*) buffer)
	   (setf (buffer-%name buffer) name))
	  (t (error "Cannot rename buffer ~S to ~S.  Name already in use."
		    buffer name)))))

(defun buffer-pathname (buffer)
  "Return a pathname for the file in Buffer.  This is the truename
  of the file as of the last time it was read or written."
  (buffer-%pathname buffer))


(defun %set-buffer-pathname (buffer pathname)
  (invoke-hook ed::buffer-pathname-hook buffer pathname)
  (setf (buffer-%pathname buffer) pathname))

(defun buffer-modeline-fields (window)
  "Return a copy of the buffer's modeline fields list."
  (do ((finfos (buffer-%modeline-fields window) (cdr finfos))
       (result () (cons (ml-field-info-field (car finfos)) result)))
      ((null finfos) (nreverse result))))

(defun %set-buffer-modeline-fields (buffer fields)
  (check-type fields list)
  (check-type buffer buffer "an editor buffer")
  (sub-set-buffer-modeline-fields buffer fields)
  (dolist (w (buffer-windows buffer))
    (update-modeline-fields buffer w)))

(defun sub-set-buffer-modeline-fields (buffer modeline-fields)
  (unless (every #'modeline-field-p modeline-fields)
    (error "Fields must be a list of modeline-field objects."))
  (setf (buffer-%modeline-fields buffer)
	(do ((fields modeline-fields (cdr fields))
	     (res nil (cons (make-ml-field-info (car fields))
			    res)))
	    ((null fields) (nreverse res)))))

(defun buffer-modeline-field-p (buffer field)
  "If field, a modeline-field or the name of one, is in buffer's list of
   modeline-fields, it is returned; otherwise, nil."
  (let ((finfo (internal-buffer-modeline-field-p buffer field)))
    (if finfo (ml-field-info-field finfo))))

(defun internal-buffer-modeline-field-p (buffer field)
  (let ((fields (buffer-%modeline-fields buffer)))
    (if (modeline-field-p field)
	(find field fields :test #'eq :key #'ml-field-info-field)
	(find field fields
	      :key #'(lambda (f)
		       (modeline-field-name (ml-field-info-field f)))))))



;;;; Variable binding -- winding and unwinding.

(eval-when (compile eval)

(defmacro unbind-variable-bindings (bindings)
  `(do ((binding ,bindings (binding-across binding)))
       ((null binding))
     (setf (car (binding-cons binding))
	   (variable-object-down (binding-object binding)))))

(defmacro bind-variable-bindings (bindings)
  `(do ((binding ,bindings (binding-across binding)))
       ((null binding))
     (let ((cons (binding-cons binding))
	   (object (binding-object binding)))
       (setf (variable-object-down object) (car cons)
	     (car cons) object))))

) ;eval-when

;;; UNWIND-BINDINGS  --  Internal
;;;
;;;    Unwind buffer variable bindings and all mode bindings up to and
;;; including mode.  Return a list of the modes unwound in reverse order.
;;; (buffer-mode-objects *current-buffer*) is clobbered.  If "mode" is NIL
;;; unwind all bindings.
;;;
(defun unwind-bindings (mode)
  (unbind-variable-bindings (buffer-var-values *current-buffer*))
  (do ((curmode (buffer-mode-objects *current-buffer*))
       (unwound ()) cw)
      (())
    (setf cw curmode  curmode (cdr curmode)  (cdr cw) unwound  unwound cw)
    (unbind-variable-bindings (mode-object-var-values (car unwound)))
    (when (or (null curmode) (eq (car unwound) mode))
      (setf (buffer-mode-objects *current-buffer*) curmode)
      (return unwound))))

;;; WIND-BINDINGS  --  Internal
;;;
;;;    Add "modes" to the mode bindings currently in effect.
;;;
(defun wind-bindings (modes)
  (do ((curmode (buffer-mode-objects *current-buffer*)) cw)
      ((null modes) (setf (buffer-mode-objects *current-buffer*) curmode))
    (bind-variable-bindings (mode-object-var-values (car modes)))
    (setf cw modes  modes (cdr modes)  (cdr cw) curmode  curmode cw))
  (bind-variable-bindings (buffer-var-values *current-buffer*)))



;;;; BUFFER-MAJOR-MODE.

(eval-when (compile eval)
(defmacro with-mode-and-buffer ((name major-p buffer) &body forms)
  `(let ((mode (get-mode-object name)))
    (setq ,name (mode-object-name mode))
    (,(if major-p 'unless 'when) (mode-object-major-p mode)
      (error "~S is not a ~:[Minor~;Major~] Mode." ,name ,major-p))
    (check-type ,buffer buffer)
    ,@forms))
) ;eval-when

;;; BUFFER-MAJOR-MODE  --  Public
;;;
;;;    The major mode is the first on the list, so just return that.
;;;
(defun buffer-major-mode (buffer)
  "Return the name of Buffer's major mode.  To change tha major mode
   use Setf."
  (check-type buffer buffer)
  (car (buffer-modes buffer)))

;;; %SET-BUFFER-MAJOR-MODE  --  Public
;;;
;;;    Unwind all modes in effect and add the major mode specified.
;;;Note that BUFFER-MODE-OBJECTS is in order of invocation in buffers
;;;other than the current buffer, and in the reverse order in the
;;;current buffer.
;;;
(defun %set-buffer-major-mode (buffer name)
  "Set the major mode of some buffer to the Name'd mode."
  (with-mode-and-buffer (name t buffer)
    (invoke-hook ed::buffer-major-mode-hook buffer name)
    (cond
     ((eq buffer *current-buffer*)
      (let ((old-mode (car (last (buffer-mode-objects buffer)))))
	(invoke-hook (%value (mode-object-hook-name old-mode)) buffer nil)
	(funcall (mode-object-cleanup-function old-mode) buffer)
	(swap-char-attributes old-mode)
	(wind-bindings (cons mode (cdr (unwind-bindings old-mode))))
	(swap-char-attributes mode)))
     (t
      (let ((old-mode (car (buffer-mode-objects buffer))))
	(invoke-hook (%value (mode-object-hook-name old-mode)) buffer nil)
	(funcall (mode-object-cleanup-function old-mode) buffer))
      (setf (car (buffer-mode-objects buffer)) mode)))
    (setf (car (buffer-modes buffer)) name)
    (funcall (mode-object-setup-function mode) buffer)
    (invoke-hook (%value (mode-object-hook-name mode)) buffer t))
  nil)



;;;; BUFFER-MINOR-MODE.

;;; BUFFER-MINOR-MODE  --  Public
;;;
;;;    Check if the mode-object is in the buffer's mode-list.
;;;
(defun buffer-minor-mode (buffer name)
  "Return true if the minor mode named Name is active in Buffer.
  A minor mode can be turned on or off with Setf."
  (with-mode-and-buffer (name nil buffer)
    (not (null (memq mode (buffer-mode-objects buffer))))))

(proclaim '(special *mode-names*))

;;; %SET-BUFFER-MINOR-MODE  --  Public
;;;
;;;    Activate or deactivate a minor mode, with due respect for
;;; bindings.
;;;
(defun %set-buffer-minor-mode (buffer name new-value)
  (let ((objects (buffer-mode-objects buffer)))
    (with-mode-and-buffer (name nil buffer)
      (invoke-hook ed::buffer-minor-mode-hook buffer name new-value)
      (cond
       ;; Already there or not there, nothing to do.
       ((if (memq mode (buffer-mode-objects buffer)) new-value (not new-value)))
       ;; Adding a new mode.
       (new-value
	(cond
	 ((eq buffer *current-buffer*)
	  ;;
	  ;; Unwind bindings having higher precedence, cons on the new
	  ;; mode and then wind them back on again.
	  (do ((m objects (cdr m))
	       (prev nil (car m)))
	      ((or (null (cdr m))
		   (< (mode-object-precedence (car m))
		      (mode-object-precedence mode)))
	       (wind-bindings
		(cons mode (if prev
			       (unwind-bindings prev)
			       (unbind-variable-bindings
				(buffer-var-values *current-buffer*))))))))
	 (t
	  (do ((m (cdr objects) (cdr m))
	       (prev objects m))
	      ((or (null m)
		   (>= (mode-object-precedence (car m))
		       (mode-object-precedence mode)))
	       (setf (cdr prev) (cons mode m))))))
	;;
	;; Add the mode name.
	(let ((bm (buffer-modes buffer)))
	  (setf (cdr bm)
		(merge 'list (cdr bm) (list name) #'<  :key
		       #'(lambda (x)
			   (mode-object-precedence (getstring x *mode-names*))))))

	(funcall (mode-object-setup-function mode) buffer)
	(invoke-hook (%value (mode-object-hook-name mode)) buffer t))
       (t
	;; Removing an active mode.
	(invoke-hook (%value (mode-object-hook-name mode)) buffer nil)
	(funcall (mode-object-cleanup-function mode) buffer)
	;; In the current buffer, unwind buffer and any mode bindings on top
	;; pop off the mode and wind the rest back on.
	(cond ((eq buffer *current-buffer*)
	       (wind-bindings (cdr (unwind-bindings mode))))
	      (t
	       (setf (buffer-mode-objects buffer)
		     (delq mode (buffer-mode-objects buffer)))))
	;; We always use the same string, so we can delq it (How Tense!)
	(setf (buffer-modes buffer) (delq name (buffer-modes buffer))))))
  new-value))



;;;; CURRENT-BUFFER, CURRENT-POINT, CURRENT-LINE, and buffer using setup
;;;; and cleanup.

(proclaim '(inline current-buffer))

(defun current-buffer ()
  "Return the current buffer object."
  *current-buffer*)

(defun current-point ()
  "Return the Buffer-Point of the current buffer."
  (buffer-point *current-buffer*))

(defun current-line ()
  "Return the line of the Buffer-Point of the current buffer."
  (mark-line (buffer-point *current-buffer*)))

(defun current-directory ()
  "Return the current directory."
  (let ((bpn (buffer-pathname (current-buffer))))
    (if bpn
	(directory-namestring bpn)
	(or (multiple-value-bind (res path)
				 (unix:unix-current-directory)
	      (if res (ed::dired-directorify path))) ;; FIX dired-directorify
	    (value pathname-defaults)))))

;;; %SET-CURRENT-BUFFER  --  Internal
;;;
;;;    Undo previous buffer and mode specific variables and character
;;;attributes and set up the new ones.  Set *current-buffer*.
;;;
(defun %set-current-buffer (buffer)
  (let ((old-buffer *current-buffer*))
    (check-type buffer buffer)
    (invoke-hook ed::set-buffer-hook buffer)
    ;; Undo old bindings.
    (setf (buffer-mode-objects *current-buffer*)
	  (unwind-bindings nil))
    (swap-char-attributes (car (buffer-mode-objects *current-buffer*)))
    (setq *current-buffer* buffer)
    (swap-char-attributes (car (buffer-mode-objects *current-buffer*)))
    ;; Make new bindings.
    (wind-bindings (shiftf (buffer-mode-objects *current-buffer*) nil))
    (invoke-hook ed::after-set-buffer-hook old-buffer))
  buffer)

;;; USE-BUFFER-SET-UP  --  Internal
;;;
;;;    This function is called by the use-buffer macro to wind on the
;;; new buffer's variable and key bindings and character attributes.
;;;
(defun use-buffer-set-up (old-buffer)
  (unless (eq old-buffer *current-buffer*)
    ;; Let new char attributes overlay old ones.
    (swap-char-attributes (car (buffer-mode-objects *current-buffer*)))
    ;; Wind on bindings of new current buffer.
    (wind-bindings (shiftf (buffer-mode-objects *current-buffer*) nil))))

;;; USE-BUFFER-CLEAN-UP  --  Internal
;;;
;;;    This function is called by use-buffer to clean up after it is done.
;;;
(defun use-buffer-clean-up (old-buffer)
  (unless (eq old-buffer *current-buffer*)
    ;; When we leave, unwind the bindings,
    (setf (buffer-mode-objects *current-buffer*) (unwind-bindings nil))
    ;; Restore the character attributes,
    (swap-char-attributes (car (buffer-mode-objects *current-buffer*)))))



;;;; Recursive editing.

(defvar *in-a-recursive-edit* nil "True if we are in a recursive edit.")

(proclaim '(inline in-recursive-edit))

(defun in-recursive-edit ()
  "Returns whether the calling point is dynamically within a recursive edit
   context."
  *in-a-recursive-edit*)

;;; RECURSIVE-EDIT  --  Public
;;;
;;;    Call the command interpreter recursively, winding on new state as
;;; necessary.
;;;
(defun recursive-edit (&optional (handle-abort t))
  "Call the command interpreter recursively.  If Handle-Abort is true then
   an abort caused by a control-g or a lisp error does not cause the
   recursive edit to be aborted."
  (invoke-hook ed::enter-recursive-edit-hook)
  (multiple-value-bind (flag args)
		       (let ((*in-a-recursive-edit* t))
			 (catch 'leave-recursive-edit
			   (if handle-abort
			       (loop (catch 'editor-top-level-catcher
				       (%command-loop)))
			       (%command-loop))))
    (case flag
      (:abort (apply #'editor-error args))
      (:exit (values-list args))
      (t (error "Bad thing ~S thrown out of recursive edit." flag)))))

;;; EXIT-RECURSIVE-EDIT is intended to be called within the dynamic context
;;; of RECURSIVE-EDIT, causing return from that function with values returned
;;; as multiple values.  When not in a recursive edit, signal an error.
;;;
(defun exit-recursive-edit (&optional values)
  "Exit from a recursive edit.  Values is a list of things which are
   to be the return values from Recursive-Edit."
  (or *in-a-recursive-edit*
      (error "Not in a recursive edit!"))
  (invoke-hook ed::exit-recursive-edit-hook values)
  (throw 'leave-recursive-edit (values :exit values)))

;;; ABORT-RECURSIVE-EDIT is intended to be called within the dynamic context
;;; of RECURSIVE-EDIT, causing EDITOR-ERROR to be called on args.  When not
;;; in a recursive edit, signal an error.
;;;
(defun abort-recursive-edit (&rest args)
  "Abort a recursive edit, causing an Editor-Error with the args given in
   the calling context."
  (or *in-a-recursive-edit*
      (error "Not in a recursive edit!"))
  (invoke-hook ed::abort-recursive-edit-hook args)
  (throw 'leave-recursive-edit (values :abort args)))



;;;; WITH-WRITABLE-BUFFER

;;; This list indicates recursive use of WITH-WRITABLE-BUFFER on the same
;;; buffer.
;;;
(defvar *writable-buffers* ())

(defmacro with-writable-buffer ((buffer) &body body)
  "Executes body in a scope where buffer is writable.  After body executes,
   this sets the buffer's modified and writable status to nil."
  (let ((buf (gensym))
	(no-unwind (gensym)))
    `(let* ((,buf ,buffer)
	    (,no-unwind (member ,buf *writable-buffers* :test #'eq))
	    (*writable-buffers* (if ,no-unwind
				    *writable-buffers*
				    (cons ,buf *writable-buffers*))))
       (unwind-protect
	   (progn
	     (setf (buffer-writable ,buf) t)
	     ,@body)
	 (unless ,no-unwind
	   (setf (buffer-modified ,buf) nil)
	   (setf (buffer-writable ,buf) nil))))))



;;;; DEFMODE.

(defun defmode (name &key (setup-function #'identity)
		     (cleanup-function #'identity) major-p transparent-p
		     precedence documentation)
  "Define a new mode, specifying whether it is a major mode, and what the
   setup and cleanup functions are.  Precedence, which defaults to 0.0, and is
   any integer or float, determines the order of the minor modes in a buffer.
   A minor mode having a greater precedence is always considered before a mode
   with lesser precedence when searching for key-bindings and variable values.
   If Transparent-p is true, then all key-bindings local to the defined mode
   are transparent, meaning that they do not shadow other bindings, but rather
   are executed in addition to them.  Documentation is used as introductory
   text for mode describing commands."
  (let ((hook-str (concatenate 'string name " Mode Hook"))
	(mode (getstring name *mode-names*)))
    (cond
     (mode
      (when (if major-p
		(not (mode-object-major-p mode))
		(mode-object-major-p mode))
	(cerror "Let bad things happen"
		"Mode ~S is being redefined as a ~:[Minor~;Major~] mode ~
		where it was ~%~
		previously a ~:*~:[Major~;Minor~] mode." name major-p))
      (warn "Mode ~S is being redefined, variables and bindings will ~
	    be preserved." name)
      (setq name (mode-object-name mode)))
     (t
      (defhvar hook-str
	       (concatenate 'string "This is the mode hook variable for "
	       name " Mode."))
      (setq mode (make-mode-object
		  :variables (make-string-table)
		  :bindings (make-hash-table)
		  :hook-name (getstring hook-str *global-variable-names*)))
      (setf (getstring name *mode-names*) mode)))

    (if precedence
	(if major-p
	    (error "Precedence ~S is meaningless for a major mode." precedence)
	    (check-type precedence number))
	(setq precedence 0))

    (setf (mode-object-major-p mode) major-p
	  (mode-object-documentation mode) documentation
	  (mode-object-transparent-p mode) transparent-p
	  (mode-object-precedence mode) precedence
	  (mode-object-setup-function mode) setup-function
	  (mode-object-cleanup-function mode) cleanup-function
	  (mode-object-name mode) name))
  nil)

(defun mode-major-p (name)
  "Returns T if Name is the name of a major mode, or NIL if is the name of
  a minor mode."
  (mode-object-major-p (get-mode-object name)))

(defun mode-variables (name)
  "Return the string-table that contains the names of the modes variables."
  (mode-object-variables (get-mode-object name)))

(defun mode-documentation (name)
  "Returns the documentation for mode with name."
  (mode-object-documentation (get-mode-object name)))



;;;; Making and Deleting buffers.

(defvar *buffer-list* () "A list of all the buffer objects.")

(defvar *current-buffer* ()
  "Internal variable which might contain the current buffer." )

(defun make-buffer (name &key (modes (value ed::default-modes))
			      (modeline-fields
			       (value ed::default-modeline-fields))
			      delete-hook)
  "Creates and returns a buffer with the given Name if a buffer with Name does
   not already exist, otherwise returns nil.  Modes is a list of mode names,
   and Modeline-fields is a list of modeline field objects.  Delete-hook is a
   list of functions that take a buffer as the argument."
  (cond ((getstring name *buffer-names*) nil)
	(t
	 (or (listp delete-hook)
	     (error ":delete-hook is a list of functions -- ~S." delete-hook))
	 (let* ((region (make-empty-region))
		(object (getstring "Fundamental" *mode-names*))
		(buffer (internal-make-buffer
			 :%name name
			 :%region region
			 :modes (list (mode-object-name object))
			 :mode-objects (list object)
			 :bindings (make-hash-table)
			 :point (copy-mark (region-end region))
			 :display-start (copy-mark (region-start region))
			 :delete-hook delete-hook
			 :variables (make-string-table))))
	   (sub-set-buffer-modeline-fields buffer modeline-fields)
	   (setf (line-%buffer (mark-line (region-start region))) buffer)
	   (push buffer *buffer-list*)
	   (setf (getstring name *buffer-names*) buffer)
	   (unless (equalp modes '("Fundamental"))
	     (setf (buffer-major-mode buffer) (car modes))
	     (dolist (m (cdr modes))
	       (setf (buffer-minor-mode buffer m) t)))
	   (invoke-hook ed::make-buffer-hook buffer)
	   buffer))))

(defun delete-buffer (buffer)
  "Deletes a buffer.  If buffer is current, or if it is displayed in any
   windows, an error is signaled."
  (when (eq buffer *current-buffer*)
    (error "Cannot delete current buffer ~S." buffer))
  (when (buffer-windows buffer)
    (error "Cannot delete buffer ~S, which is displayed in ~R window~:P."
	   buffer (length (buffer-windows buffer))))
  (invoke-hook (buffer-delete-hook buffer) buffer)
  (invoke-hook ed::delete-buffer-hook buffer)
  (setq *buffer-list* (delq buffer *buffer-list*))
  (delete-string (buffer-name buffer) *buffer-names*)
  nil)

(defun unique-buffer-name (name)
  "Return the next unique buffer name like Name.  If Name ends in a space
   and an integer then increment the integer until a new buffer name is
   found, otherwise append the lowest positive integer that makes a new
   buffer name."
  (if (getstring name *buffer-names*)
      (let* ((pos (position #\  name :from-end t))
	     (start (if pos
			(parse-integer name :start pos :junk-allowed t)))
	     (buffer-name (if start (subseq name 0 pos) name)))
	(loop
	  for num = (if start (1+ start) 1) then (1+ num)
	  for name = (format nil "~A ~D" buffer-name num)
	  then (format nil "~A ~D" buffer-name num)
	  while (getstring name *buffer-names*)
	  finally return name))
      name))

(defun copy-buffer (buffer)
  "Return a copy of Buffer."
  (let ((new-name (unique-buffer-name (buffer-name buffer)))
	(new-buffer (internal-copy-buffer buffer)))
    (setf (buffer-%name new-buffer) new-name)

    (let ((region (make-empty-region)))
      (setf (buffer-%region new-buffer) region)
      (setf (line-%buffer (mark-line (region-start region)))
	    new-buffer))
    (if (buffer-writable new-buffer)
	(setf (buffer-region new-buffer)
	      (copy-region (buffer-region buffer)))
	(with-writable-buffer (new-buffer)
	  (setf (buffer-region new-buffer)
		(copy-region (buffer-region buffer)))))

    ;; Copy the buffer point position and kind.
    (let ((mark (copy-mark (buffer-start-mark new-buffer) :left-inserting))
	  (buffer-point (buffer-point buffer)))
      (line-offset mark (1- (count-lines
			     (region (buffer-start-mark buffer)
				     buffer-point))))
      (setf (mark-charpos mark) (mark-charpos buffer-point))
      (setf (mark-kind mark) (mark-kind buffer-point))
      (setf (buffer-point new-buffer) mark))

    (setf (buffer-windows new-buffer) nil)

    ;; Copy the display start position and kind.
    (let ((mark (copy-mark (buffer-start-mark new-buffer) :left-inserting))
	  (buffer-ds (buffer-display-start buffer)))
      (line-offset mark (1- (count-lines
			     (region (buffer-start-mark buffer)
				     (buffer-display-start buffer)))))
      (setf (mark-charpos mark) (mark-charpos buffer-ds))
      (setf (mark-kind mark) (mark-kind buffer-ds))
      (setf (buffer-display-start new-buffer) mark))

    ;; Copy variables.
    (setf (buffer-variables new-buffer) (make-string-table))
    (setf (buffer-var-values new-buffer) nil)
    (loop for var = (buffer-var-values buffer)
                  then (aref var 2)
          while var do
      (let ((var-obj (aref var 1)))
	(defhvar (variable-object-name var-obj)
	  (variable-object-documentation var-obj)
	  :buffer new-buffer
	  :value (variable-object-value var-obj))))

    ;; Setup modes.
    (let ((object (getstring "Fundamental" *mode-names*)))
      (setf (buffer-modes new-buffer) (list (mode-object-name object)))
      (setf (buffer-mode-objects new-buffer) (list object)))
    (setf (buffer-major-mode new-buffer) (buffer-major-mode buffer))
    (dolist (mode (cdr (buffer-modes buffer)))
      (setf (buffer-minor-mode new-buffer mode) t))

    ;; Copy marks.
    (do ((num 0 (1+ num))
	 (line (mark-line (buffer-start-mark buffer))
	       (line-next line))
	 (new-line (mark-line (buffer-start-mark new-buffer))
		   (line-next new-line))
	 (last-line (mark-line (buffer-end-mark buffer))))
	((eq line last-line))
      (dolist (mark (line-marks line))
	(if (fast-font-mark-p mark)
	    (font-mark new-line
		       (mark-charpos mark)
		       (font-mark-font mark)
		       (mark-kind mark))
	    (mark new-line (mark-charpos mark) (mark-kind mark)))))

    (push new-buffer *buffer-list*)
    (setf (getstring new-name *buffer-names*) new-buffer)
    (invoke-hook ed::make-buffer-hook new-buffer)
    new-buffer))

(defun make-unique-buffer (name &key (modes (value ed::default-modes))
				     (modeline-fields
				      (value ed::default-modeline-fields))
				     delete-hook)
  (loop
    for num = 1 then (1+ num)
    for buffer = (make-buffer name
			      :modes modes
			      :modeline-fields modeline-fields
			      :delete-hook delete-hook)
        then (make-buffer (format nil "~A ~D" name num)
			  :modes modes
			  :modeline-fields modeline-fields
			  :delete-hook delete-hook)
    do
    (if buffer (return-from make-unique-buffer buffer))))



;;;; Buffer start and end marks.

(defun buffer-start-mark (buffer)
  "Returns the buffer-region's start mark."
  (region-start (buffer-region buffer)))

(defun buffer-end-mark (buffer)
  "Returns the buffer-region's end mark."
  (region-end (buffer-region buffer)))



;;;; Setting up initial buffer.

;;; SETUP-INITIAL-BUFFER  --  Internal
;;;
;;;    Create the buffer "Main" and the mode "Fundamental".  We make a
;;; dummy fundamental mode before we make the buffer Main, because
;;; "make-buffer" wants fundamental to be defined when it is called, and we
;;; can't make the real fundamental mode until there is a current buffer
;;; because "defmode" wants to invoke it's mode definition hook.  Also,
;;; when creating the "Main" buffer, "Default Modeline Fields" is not yet
;;; defined, so we supply this argument to MAKE-BUFFER as nil.  This is
;;; fine since firing up the editor in a core must set the "Main" buffer's
;;; modeline according to this variable in case the user changed it in his
;;; init file.  After the main buffer is created we then define the real
;;; fundamental mode and bash it into the buffer.
;;;
(defun setup-initial-buffer ()
  ;; Make it look like the mode is there so make-buffer doesn't die.
  (setf (getstring "Fundamental" *mode-names*)
	(make-mode-object :major-p t))
  ;; Make it look like there is a make-buffer-hook...
  (setf (get 'ed::make-buffer-hook 'hemlock-variable-value)
	(make-variable-object "foo" "bar"))
  (setq *current-buffer* (make-buffer "Main" :modes '("Fundamental")
				      :modeline-fields nil))
  (or *current-buffer*
      (error "Failed to init *current-buffer*."))
  ;; Make the bogus variable go away...
  (remf (symbol-plist 'ed::make-buffer-hook) 'hemlock-variable-value)
  ;; Make it go away so defmode doesn't die.
  (setf (getstring "Fundamental" *mode-names*) nil)
  (defmode "Fundamental" :major-p t)
  ;; Bash the real mode object into the buffer.
  (let ((obj (getstring "Fundamental" *mode-names*)))
    (setf (car (buffer-mode-objects *current-buffer*)) obj
	  (car (buffer-modes *current-buffer*)) (mode-object-name obj))))
