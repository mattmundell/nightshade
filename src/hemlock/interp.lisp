;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; Routines which define commands, and the command interpreter.

(in-package "HEMLOCK-INTERNALS")

(export '(bind-key delete-key-binding get-command map-bindings
	  make-command command-name command-bindings
	  visible-command-bindings last-command-type
	  prefix-argument exit-hemlock *invoke-hook* key-translation))


(defun %print-hcommand (obj stream depth)
  (declare (ignore depth))
  (write-string "#<Hemlock Command \"" stream)
  (write-string (command-name obj) stream)
  (write-string "\">" stream))



;;;; Key Tables:
;;;
;;;    A key table provides a way to translate a sequence of characters to some
;;; lisp object.  It is currently represented by a tree of hash-tables, where
;;; each level is a hashing from a key to either another hash-table or a value.


;;; GET-TABLE-ENTRY returns the value at the end of a series of hashings.  For
;;; our purposes it is presently used to look up commands and key-translations.
;;;
(defun get-table-entry (table key)
  (let ((foo nil))
    (dotimes (i (length key) foo)
      (let ((key-event (aref key i)))
	(setf foo (gethash key-event table))
	(unless (hash-table-p foo) (return foo))
	(setf table foo)))))

;;; SET-TABLE-ENTRY sets the entry for key in table to val, creating new
;;; tables as needed.  If val is nil, then use REMHASH to remove this element
;;; from the hash-table.
;;;
(defun set-table-entry (table key val)
  (dotimes (i (1- (length key)))
    (let* ((key-event (aref key i))
	   (foo (gethash key-event table)))
      (if (hash-table-p foo)
	  (setf table foo)
	  (let ((new-table (make-hash-table)))
	    (setf (gethash key-event table) new-table)
	    (setf table new-table)))))
  (if (null val)
      (remhash (aref key (1- (length key))) table)
      (setf (gethash (aref key (1- (length key))) table) val)))


;;;; Key Translation:
;;;
;;;    Key translations are maintained using a key table.  If a value is an
;;; integer, then it is prefix bits to be OR'ed with the next character.  If it
;;; is a key, then we translate to that key.

(defvar *key-translations* (make-hash-table))
(defvar *translate-key-temp* (make-array 10 :fill-pointer 0 :adjustable t))


;;; TRANSLATE-KEY  --  Internal
;;;
;;;    This is used internally to do key translations when we want the
;;; canonical representation for Key.  Result, if supplied, is an adjustable
;;; vector with a fill pointer.  We compute the output in this vector.  If the
;;; key ends in the prefix of a translation, we just return that part
;;; untranslated and return the second value true.
;;;
(defun translate-key (key &optional (result (make-array (length key)
							:fill-pointer 0
							:adjustable t)))
  (let ((key-len (length key))
	(temp *translate-key-temp*)
	(start 0)
	(try-pos 0)
	(prefix 0))
    (setf (fill-pointer temp) 0)
    (setf (fill-pointer result) 0)
    (loop
      (when (= try-pos key-len) (return))

      ;; FIX If at the end of a hyper prefix set `prefix' 0 and check whether
      ;; the next key begins a hyper prefix.  If so carry on (the following
      ;; hyper can do the prefixing), otherwise revert `prefix' and
      ;; create the key event with the reverted value (the event will be
      ;; the actual hyper-modified key).

      (let ((key-event (aref key try-pos)))
	(vector-push-extend
	 (ext:make-key-event key-event (logior (ext:key-event-bits key-event)
					       prefix))
	 temp)
	(setf prefix 0))
      (let ((entry (get-table-entry *key-translations* temp)))
	(cond ((hash-table-p entry)
	       (incf try-pos))
	      (t
	       (etypecase entry
		 (null
		  (vector-push-extend (aref temp 0) result)
		  (incf start))
		 (simple-vector
		  (dotimes (i (length entry))
		    (vector-push-extend (aref entry i) result))
		  (setf start (1+ try-pos)))
		 (integer
		  (setf start (1+ try-pos))
		  (when (= start key-len) (return))
		  ;; FIX why respect existing prefix if existing prefix set 0 above?
		  (setf prefix (logior entry prefix))))
	       (setq try-pos start)
	       (setf (fill-pointer temp) 0)))))
    (dotimes (i (length temp))
      (vector-push-extend (aref temp i) result))
    (values result (not (zerop (length temp))))))


;;; KEY-TRANSLATION -- Public.
;;;
(defun key-translation (key)
  "Return the key translation for Key, or NIL if there is none.  If Key is a
   prefix of a translation, then :Prefix is returned.  Whenever Key appears as a
   subsequence of a key argument to the binding manipulation functions, that
   portion will be replaced with the translation.  A key translation may also be
   a list (:Bits {Bit-Name}*).  In this case, the named bits will be set in the
   next character in the key being translated."
  (let ((entry (get-table-entry *key-translations* (crunch-key key))))
    (etypecase entry
      (hash-table :prefix)
      ((or simple-vector null) entry)
      (integer
       (cons :bits (ext:key-event-bits-modifiers entry))))))

;;; %SET-KEY-TRANSLATION  --  Internal
;;;
(defun %set-key-translation (key new-value)
  (let ((entry (cond ((and (consp new-value) (eq (car new-value) :bits))
		      (apply #'ext:make-key-event-bits (cdr new-value)))
		     (new-value (crunch-key new-value))
		     (t new-value))))
    (set-table-entry *key-translations* (crunch-key key) entry)
    new-value))
;;;
(defsetf key-translation %set-key-translation
  "Set the key translation for a key.  If set to null, deletes any
  translation.")



;;;; Interface Utility Functions:

(defvar *global-command-table* (make-hash-table)
  "The command table for global key bindings.")

;;; GET-RIGHT-TABLE  --  Internal
;;;
;;;    Return a hash-table depending on "kind" and checking for errors.
;;;
(defun get-right-table (kind where)
  (case kind
     (:global
      (when where
	(error "Where argument ~S is meaningless for :global bindings."
	       where))
      *global-command-table*)
     (:mode (let ((mode (getstring where *mode-names*)))
	      (unless mode
		(error "~S is not a defined mode." where))
	      (mode-object-bindings mode)))
     (:buffer (unless (bufferp where)
		(error "~S is not a buffer." where))
	      (buffer-bindings where))
     (t (error "~S is not a valid binding type." kind))))


;;; CRUNCH-KEY  --  Internal.
;;;
;;; Take a key in one of the various specifications and turn it into the
;;; standard one: a simple-vector of characters.
;;;
(defun crunch-key (key)
  (typecase key
    (ext:key-event (vector key))
    ((or list vector) ;List thrown in gratuitously.
     (when (zerop (length key))
       (error "A zero length key is illegal."))
     (unless (every #'ext:key-event-p key)
       (error "A Key ~S must contain only key-events." key))
     (coerce key 'simple-vector))
    (t
     (error "Key ~S is not a key-event or sequence of key-events." key))))



;;;; Exported Primitives:

(declaim (special *command-names*))

;;; BIND-KEY  --  Public.
;;;
(defun bind-key (name key &optional (kind :global) where)
  "Bind a Hemlock command to some key somewhere.  Name is the string name
   of a Hemlock command, Key is either a key-event or a vector of key-events.
   Kind is one of :Global, :Mode or :Buffer, and where is the mode name or
   buffer concerned.  Kind defaults to :Global."
  (let ((cmd (getstring name *command-names*))
	(table (get-right-table kind where))
	(key (copy-seq (translate-key (crunch-key key)))))
    (cond (cmd
	   (set-table-entry table key cmd)
	   (push (list key kind where) (command-%bindings cmd))
	   cmd)
	  (t
	   (with-simple-restart (continue "Go on, ignoring binding attempt.")
	     (error "~S is not a defined command." name))))))


;;; DELETE-KEY-BINDING  --  Public
;;;
;;;    Stick NIL in the key table specified.
;;;
(defun delete-key-binding (key &optional (kind :global) where)
  "Remove a Hemlock key binding somewhere.  Key is either a key-event or a
   vector of key-events.  Kind is one of :Global, :Mode or :Buffer, andl where
   is the mode name or buffer concerned.  Kind defaults to :Global."
  (set-table-entry (get-right-table kind where)
		   (translate-key (crunch-key key))
		   nil))


;;; GET-CURRENT-BINDING  --  Internal
;;;
;;;    Look up a key in the current environment.
;;;
(defun get-current-binding (key)
  (let ((res (get-table-entry (buffer-bindings *current-buffer*) key)))
    (cond
     (res (values res nil))
     (t
      (do ((mode (buffer-mode-objects *current-buffer*) (cdr mode))
	   (t-bindings ()))
	  ((null mode)
	   (values (get-table-entry *global-command-table* key)
		   (nreverse t-bindings)))
	(declare (list t-bindings))
	(let ((res (get-table-entry (mode-object-bindings (car mode)) key)))
	  (when res
	    (if (mode-object-transparent-p (car mode))
		(push res t-bindings)
		(return (values res (nreverse t-bindings)))))))))))


;;; GET-COMMAND -- Public.
;;;
(defun get-command (key &optional (kind :global) where)
  "Return the command object for the command bound to key somewhere.
   If key is not bound, return nil.  Key is either a key-event or a vector of
   key-events.  If key is a prefix of a key-binding, then return :prefix.
   Kind is one of :global, :mode or :buffer, and where is the mode name or
   buffer concerned.  Kind defaults to :Global."
  (multiple-value-bind (key prefix-p)
		       (translate-key (crunch-key key))
    (let ((entry (if (eq kind :current)
		     (get-current-binding key)
		     (get-table-entry (get-right-table kind where) key))))
      (etypecase entry
	(null (if prefix-p :prefix nil))
	(command entry)
	(hash-table :prefix)))))

(defvar *map-bindings-key* (make-array 5 :adjustable t :fill-pointer 0))

;;; MAP-BINDINGS -- Public.
;;;
(defun map-bindings (function kind &optional where)
  "Map function over the bindings in some place.  The function is passed the
   key and the command to which it is bound."
  (labels ((mapping-fun (hash-key hash-value)
	     (vector-push-extend hash-key *map-bindings-key*)
	     (etypecase hash-value
	       (command (funcall function *map-bindings-key* hash-value))
	       (hash-table (maphash #'mapping-fun hash-value)))
	     (decf (fill-pointer *map-bindings-key*))))
    (setf (fill-pointer *map-bindings-key*) 0)
    (maphash #'mapping-fun (get-right-table kind where))))

;;; MAKE-COMMAND -- Public.
;;;
;;; If the command is already defined, then alter the command object;
;;; otherwise, make a new command object and enter it into the *command-names*.
;;;
(defun make-command (name documentation function)
  "Create a new Hemlock command with Name and Documentation which is
   implemented by calling the function-value of the symbol Function"
  (let ((entry (getstring name *command-names*)))
    (cond
     (entry
      (setf (command-name entry) name)
      (setf (command-documentation entry) documentation)
      (setf (command-function entry) function))
     (t
      (setf (getstring name *command-names*)
	    (internal-make-command name documentation function))))))


;;; COMMAND-NAME, %SET-COMMAND-NAME -- Public.
;;;
(defun command-name (command)
  "Returns the string which is the name of Command."
  (command-%name command))
;;;
(defun %set-command-name (command new-name)
  (check-type command command)
  (check-type new-name string)
  (setq new-name (coerce new-name 'simple-string))
  (delete-string (command-%name command) *command-names*)
  (setf (getstring new-name *command-names*) command)
  (setf (command-%name command) new-name))


;;; COMMAND-BINDINGS -- Public.
;;;
;;; Check that all the supposed bindings really exists.  Bindings which
;;; were once made may have been overwritten.  It is easier to filter
;;; out bogus bindings here than to catch all the cases that can make a
;;; binding go away.
;;;
(defun command-bindings (command)
  "Return a list of lists of the form (key kind where) describing
   all the places where Command is bound."
  (check-type command command)
  (let (result)
    (declare (list result))
    (dolist (place (command-%bindings command))
      (let ((table (case (cadr place)
		   (:global *global-command-table*)
		   (:mode
		    (let ((m (getstring (caddr place) *mode-names*)))
		      (when m (mode-object-bindings m))))
		   (t
		    (when (memq (caddr place) *buffer-list*)
		      (buffer-bindings (caddr place)))))))
	(when (and table
		   (eq (get-table-entry table (car place)) command)
		   (not (member place result :test #'equalp)))
	  (push place result))))
    result))

;;; VISIBLE-COMMAND-BINDINGS -- Public.
;;;
(defun visible-command-bindings (command)
  "Return a list of lists of the form (key kind where) describing all the
   visible places where Command is bound, i.e. the global bindings, the
   bindings in the current modes and the bindings in the current buffer."
  (check-type command command)
  (let ((buffer (current-buffer)) result)
    (declare (list result))
    (dolist (place (command-%bindings command))
      (let ((table (case (cadr place)
		     (:global *global-command-table*)
		     (:mode
		      (let* ((name (caddr place))
			     (m (getstring name *mode-names*)))
			(when (and m
				   (or (string= (string-downcase (buffer-major-mode buffer))
						(string-downcase name))
				       (if (mode-major-p name)
					   nil
					   (buffer-minor-mode buffer name))))
			  (mode-object-bindings m))))
		     (t
		      (when (eq (caddr place) buffer)
			(buffer-bindings (caddr place)))))))
	(when (and table
		   (eq (get-table-entry table (car place)) command)
		   (not (member place result :test #'equalp)))
	  (push place result))))
    result))


(defvar *last-command-type* ()
  "The command-type of the last command invoked.")
(defvar *command-type-set* ()
  "True if the last command set the command-type.")

;;; LAST-COMMAND-TYPE  --  Public
;;;
;;;
(defun last-command-type ()
  "Return the command-type of the last command invoked.
  If no command-type has been set then return NIL.  Setting this with
  Setf sets the value for the next command."
  *last-command-type*)

;;; %SET-LAST-COMMAND-TYPE  --  Internal
;;;
;;;    Set the flag so we know not to clear the command-type.
;;;
(defun %set-last-command-type (type)
  (setq *last-command-type* type *command-type-set* t))


(defvar *prefix-argument* nil "The prefix argument or NIL.")
(defvar *prefix-argument-supplied* nil
  "Should be set by functions which supply a prefix argument.")

;;; PREFIX-ARGUMENT  --  Public
;;;
;;;
(defun prefix-argument ()
  "Return the current value of prefix argument.  This can be set with SETF."
  *prefix-argument*)

;;; %SET-PREFIX-ARGUMENT  --  Internal
;;;
(defun %set-prefix-argument (argument)
  "Set the prefix argument for the next command to Argument."
  (unless (or (null argument) (integerp argument))
    (error "Prefix argument ~S is neither an integer nor Nil." argument))
  (setq *prefix-argument* argument  *prefix-argument-supplied* t))

;;;; The Command Loop:

;;; Buffers we use to read and translate keys.
;;;
(defvar *current-command* (make-array 10 :fill-pointer 0 :adjustable t))
(defvar *current-translation* (make-array 10 :fill-pointer 0 :adjustable t))

(defvar *invoke-hook* #'(lambda (command p)
			  (funcall (command-function command) p))
  "This function is called by the command interpreter when it wants to invoke a
  command.  The arguments are the command to invoke and the prefix argument.
  The default value just calls the Command-Function with the prefix argument.")

;;; %COMMAND-LOOP  --  Internal
;;;
;;;    Read commands from the terminal and execute them, forever.
;;;
(defun %command-loop ()
  (let  ((cmd *current-command*)
	 (trans *current-translation*)
	 (*last-command-type* nil)
	 (*command-type-set* nil)
	 (*prefix-argument* nil)
	 (*prefix-argument-supplied* nil))
    (declare (special *last-command-type* *command-type-set*
		      *prefix-argument* *prefix-argument-supplied*))
    (setf (fill-pointer cmd) 0)
    (handler-bind
	;; Bind this outside the invocation loop to save consing.
	((editor-error #'(lambda (condx)
			   (beep)
			   (let ((string (editor-error-format-string condx)))
			     (when string
			       (apply #'message string
				      (editor-error-format-arguments condx)))
			     (throw 'command-loop-catcher nil)))))
      (loop
	(unless (eq *current-buffer* *echo-area-buffer*)
	  (when (buffer-modified *echo-area-buffer*) (clear-echo-area))
	  (unless (or (zerop (length cmd))
		      (not (value ed::key-echo-delay)))
	    (editor-sleep (value ed::key-echo-delay))
	    (unless (listen-editor-input *editor-input*)
	      (clear-echo-area)
; for hyper
;	      (format *echo-area-stream* "(~A)" (length cmd))
;	      (format *echo-area-stream* "eq 0 (~A) #\escape: ~A" (elt cmd 0) (eq (elt cmd 0) #\escape))
; 	      (if (and (> (length cmd) 4)
; 		       (eq (key-event-char (elt cmd 0) ) #\escape)
; 		       (eq (key-event-char (elt cmd 1) ) #\[)
; 		       (eq (key-event-char (elt cmd 2) ) #\2)
; 		       (eq (key-event-char (elt cmd 3) ) #\5)
; 		       (eq (key-event-char (elt cmd 4) ) #\~))
; 		  (if (and (> (length cmd) 9))
; 		      (write-string "H+-" *echo-area-stream*)
; 		      (write-string "H-" *echo-area-stream*))
		  (dotimes (i (length cmd))
		    (ext:print-pretty-key (aref cmd i) *echo-area-stream*)
		    (write-char #\space *echo-area-stream*))
;)
	      )))
	(vector-push-extend (get-key-event *editor-input*) cmd)
	(multiple-value-bind (trans-result prefix-p)
			     (translate-key cmd trans)
	  (multiple-value-bind (res t-bindings)  ;; transparent-bindings
			       (get-current-binding trans-result)
	    (etypecase res
	      (command
	       (let ((punt t))
		 (catch 'command-loop-catcher
		   (let ((*changed-buffers* nil))
		     (dolist (c t-bindings)
		       (funcall *invoke-hook* c *prefix-argument*))
		     (funcall *invoke-hook* res *prefix-argument*)
		     (dolist (buffer *changed-buffers*)
		       (when (member buffer *buffer-list*)
			 (invoke-hook ed::after-change-hook buffer))))
		   (setf punt nil))
		 (when punt (invoke-hook ed::command-abort-hook))
		 (invoke-hook ed::after-command-hook))
	       (if *command-type-set*
		   (setq *command-type-set* nil)
		   (setq *last-command-type* nil))
	       (if *prefix-argument-supplied*
		   (setq *prefix-argument-supplied* nil)
		   (setq *prefix-argument* nil))
	       (setf (fill-pointer cmd) 0))
	      (null
	       (unless prefix-p
		 (beep)
		 (setq *prefix-argument* nil)
		 (setf (fill-pointer cmd) 0)))
	      (hash-table))))))))


;;; EXIT-HEMLOCK  --  Public
;;;
(defun exit-hemlock (&optional (value t))
  "Exit from ED, returning the specified value."
  (throw 'hemlock-exit value))
