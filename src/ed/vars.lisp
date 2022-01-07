;;; The routines which define editor variables.

(in-package "EDI")

(export '(variable-value variable-hooks variable-documentation variable-name
	  variable-source
	  editor-bound-p delete-variable))


#[ Editor Variables

Editor variables supply a simple customization mechanism by permitting
commands to be parameterized.  For details see [ vars ].

As an example, the following variable is named `Bell Style' and it's
initial value is the keyword :border-flash.  The documentation string
describes the variable and the possible values it may take.

{evariable:Bell Style}
]#

#[ Editor Variables (extension)

The editor implements a system of variables separate from normal Lisp
variables for the following reasons:

  1) The editor has different scoping rules which are useful in an editor.  Editor
     variables can be global, or local to either [buffers] or [modes].

  2) Editor variables have [hooks], lists of functions
     called when someone sets the variable.  See variable-value for the
     arguments the editor passes to these hook functions.

  3) There is a database of variable names and documentation which makes it easier
     to find out what variables exist and what their values mean.

[ Variable Names     ]
[ Variable Functions ]
[ Hooks              ]
]#

#[ Variable Names

At the interface, a variable name is a case-folded string.  This string is
referred to as the string name of the variable.  A string name is
conventionally composed of words separated by spaces.

In Lisp code a variable name is a symbol.  The name of this symbol is
created by replacing any spaces in the string name with hyphens.  This
symbol name is always interned in the "ed" package and referring
to a symbol with the same name in the wrong package is an error.

{variable:ed:*global-variable-names*}
{function:ed:current-variable-tables}
]#

#[ Variable Functions

In the following descriptions name is the symbol name of the variable.

{function:ed:defevar}
{function:ed:variable-value}
{function:ed:variable-documentation}
{function:ed:variable-hooks}
{function:ed:variable-name}
{function:ed:string-to-variable}
{function:ed:value}
{function:ed:setv}
{function:ed:elet}
{function:ed:editor-bound-p}
{function:ed:delete-variable}
{evariable:Delete Variable Hook}
]#


(defstruct (binding
	    (:type vector)
	    (:copier nil)
	    (:constructor make-binding (cons object across symbol)))
  cons		; The cons which holds the value for the property.
  object	; The variable-object for the binding.
  across        ; The next binding in this place.
  symbol)	; The symbol name for the variable bound.

;;; UNDEFINED-VARIABLE-ERROR  --  Internal
;;;
;;; Complain about an undefined editor variable in a helpful fashion.
;;;
(defun undefined-variable-error (name)
  (if (eq (symbol-package name) (find-package "ED"))
      (error "Undefined editor variable ~A." name)
      (error "Editor variables must be in the \"ED\" package,~%~
	     ~S is in the ~S package."
	     name (package-name (symbol-package name)))))

;;; GET-MODE-OBJECT  --  Internal
;;;
;;; Get the mode-object corresponding to name or die trying.
;;;
(defun get-mode-object (name)
  (or (stringp name) (error "Mode name ~S is not a string." name))
  (let ((res (getstring name *mode-names*)))
    (or res (error "~S is not a defined mode." name))
    res))

;;; FIND-BINDING  --  Internal
;;;
;;; Return the Binding object corresponding to Name in the collection of
;;; bindings Binding, or NIL if none.
;;;
(defun find-binding (name binding)
  (do ((b binding (binding-across b)))
      ((null b) nil)
    (when (eq (binding-symbol b) name) (return b))))

;;; GET-VARIABLE-OBJECT  --  Internal
;;;
;;; Get the variable-object with the specified symbol-name, kind and where,
;;; or die trying.
;;;
(defun get-variable-object (name kind where)
  (case kind
    (:current
     (let ((obj (get name 'editor-variable-value)))
       (if obj obj (undefined-variable-error name))))
    (:buffer
     (check-type where buffer)
     (let ((binding (find-binding name (buffer-var-values where))))
       (or binding
	   (error "~S is not a defined editor variable in buffer ~S." name where))
       (binding-object binding)))
    (:global
     (do ((obj (get name 'editor-variable-value)
	       (variable-object-down obj))
	  (prev nil obj))
	 ((symbolp obj)
	  (or prev (undefined-variable-error name))
	  (or (eq obj :global)
	      (error "Editor variable ~S is not globally defined." name))
	  prev)))
    (:mode
     (let ((binding (find-binding name (mode-object-var-values
					(get-mode-object where)))))
       (or binding
	   (error "~S is not a defined editor variable in mode ~S." name where))
       (binding-object binding)))
    (t
     (error "~S is not a defined value for Kind." kind))))

;;; VARIABLE-VALUE  --  Public
;;;
;;; Get the value of the editor variable "name".
;;;
(defun variable-value (name &optional (kind :current) where)
  "Return the value of editor variable $name in some place.  The following
   values for $kind are defined:

     :current
	Return the value present in the current environment, taking into
	consideration any mode or buffer local variables.

     :global
	 Return the global value.

     :mode
	 Return the value in the mode named $where.

     :buffer

	 Return the value in the buffer $where.

   When set with setf, set the value of the specified variable and invoke
   the functions in its hook list with name, kind, where and the new
   value."
  (variable-object-value (get-variable-object name kind where)))

;;; %VALUE  --  Internal
;;;
;;; This function is called by the expansion of Value.
;;;
(defun %value (name)
  (let ((obj (get name 'editor-variable-value)))
    (or obj (undefined-variable-error name))
    (variable-object-value obj)))

;;; %SET-VALUE  --  Internal
;;;
;;; The setf-inverse of Value, set the current value.
;;;
(defun %set-value (var new-value)
  (let ((obj (get var 'editor-variable-value)))
    (or obj (undefined-variable-error var))
    (invoke-hook (variable-object-hooks obj) var :current nil new-value)
    (setf (variable-object-value obj) new-value)))

;;; %SET-VARIABLE-VALUE  --  Internal
;;;
;;; Set the Editor variable with the symbol name "name".
;;;
(defun %set-variable-value (name kind where new-value)
  (let ((obj (get-variable-object name kind where)))
    (invoke-hook (variable-object-hooks obj) name kind where new-value)
    (setf (variable-object-value obj) new-value)))

;;; VARIABLE-HOOKS  --  Public
;;;
;;; Return the list of hooks for "name".
;;;
(defun variable-hooks (name &optional (kind :current) where)
  "Return the list of hook functions of editor variable $name.

   $kind and $where are the same as for `variable-value'.

   May be set using `setf'."
  (variable-object-hooks (get-variable-object name kind where)))

;;; %SET-VARIABLE-HOOKS --  Internal
;;;
;;; Set the hook-list for editor variable Name.
;;;
(defun %set-variable-hooks (name kind where new-value)
  (setf (variable-object-hooks (get-variable-object name kind where)) new-value))

;;; VARIABLE-DOCUMENTATION  --  Public
;;;
;;; Return the documentation for "name".
;;;
(defun variable-documentation (name &optional (kind :current) where)
  "Return the documentation of editor variable $name.

   $kind and $where are the same as for `variable-value'.

   May be set using `setf'."
  (variable-object-documentation (get-variable-object name kind where)))

;;; %SET-VARIABLE-DOCUMENTATION  --  Internal
;;;
;;; Set a variables documentation.
;;;
(defun %set-variable-documentation (name kind where new-value)
  (setf (variable-object-documentation (get-variable-object name kind where))
	new-value))

;;; VARIABLE-SOURCE  --  Public
;;;
;;; Return the Source of an editor variable.
;;;
(defun variable-source (name &optional (kind :current) where)
  "Return the source of an editor variable."
  (variable-object-source (get-variable-object name kind where)))

;;; %SET-VARIABLE-SOURCE  --  Internal
;;;
;;; Set a variables source.
;;;
(defun %set-variable-source (name kind where new-value)
  (setf (variable-object-source (get-variable-object name kind where))
	new-value))

;;; VARIABLE-NAME  --  Public
;;;
;;; Return the String Name for an editor variable.
;;;
(defun variable-name (name &optional (kind :current) where)
  "Return the string name of editor variable $name.

   $kind and $where are the same as for `variable-value'.

   May be set using `setf'."
  (variable-object-name (get-variable-object name kind where)))

;;; EDITOR-BOUND-P  --  Public
;;;
(defun editor-bound-p (name &optional (kind :current) where)
  "Return t if $name is defined as an editor variable in place specified by
   $kind and $where, else return ().

   $kind and $where are the same as for `variable-value'."
  (case kind
    (:current (not (null (get name 'editor-variable-value))))
    (:buffer
     (check-type where buffer)
     (not (null (find-binding name (buffer-var-values where)))))
    (:global
     (do ((obj (get name 'editor-variable-value)
	       (variable-object-down obj)))
	 ((symbolp obj) (eq obj :global))))
    (:mode
     (not (null (find-binding name (mode-object-var-values
				    (get-mode-object where))))))))

(defun string-to-variable (string)
  "Return the symbol name of an editor variable from the corresponding
   string name."
  (intern (nsubstitute #\- #\space (the simple-string (string-upcase string)))
	  (find-package "ED")))

(defvar *global-variable-names* (make-string-table)
  "A string table of the names of all global editor variables.  The value
   of each entry is the symbol name of the variable.")

(defun %defevar (name documentation mode buffer hooks value source)
  (let* ((symbol-name (string-to-variable name))
	 (new-binding (make-variable-object documentation name))
	 (plist (symbol-plist symbol-name))
	 (prop (cdr (or (memq 'editor-variable-value plist)
			(setf (symbol-plist symbol-name)
			      (list* 'editor-variable-value nil plist)))))
	 (kind :global) where string-table)
    (cond
      (mode
       (setq kind :mode  where mode)
       (let* ((obj (get-mode-object where))
	      (vars (mode-object-var-values obj)))
	 (setq string-table (mode-object-variables obj))
	 (or (find-binding symbol-name vars)
	     (let ((binding (make-binding prop new-binding vars symbol-name)))
	       (cond ((memq obj (buffer-mode-objects *current-buffer*))
		      (let ((l (unwind-bindings obj)))
			(setf (mode-object-var-values obj) binding)
			(wind-bindings l)))
		     (t
		      (setf (mode-object-var-values obj) binding)))))))
      (buffer
       (check-type buffer buffer)
       (setq kind :buffer  where buffer  string-table (buffer-variables buffer))
       (let ((vars (buffer-var-values buffer)))
	 (or (find-binding symbol-name vars)
	     (let ((binding (make-binding prop new-binding vars symbol-name)))
	       (setf (buffer-var-values buffer) binding)
	       (when (eq buffer *current-buffer*)
		 ;; FIX what does this do?
		 (setf (variable-object-down new-binding) (car prop)
		       (car prop) new-binding))))))
      (t
       (setq string-table *global-variable-names*)
       (unless (editor-bound-p symbol-name :global)
	 (setf (variable-object-down new-binding) :global)
	 (let ((l (unwind-bindings nil)))
	   (setf (car prop) new-binding)
	   (wind-bindings l)))))
    (setf (getstring name string-table) symbol-name)
    (setf (variable-hooks symbol-name kind where) hooks)
    (setf (variable-value symbol-name kind where) value)
    (setf (variable-source symbol-name kind where) source))
  name)

;;; DELETE-BINDING  --  Internal
;;;
;;; Delete a binding from a list of bindings.
;;;
(defun delete-binding (binding bindings)
  (do ((b bindings (binding-across b))
       (prev nil b))
      ((eq b binding)
       (cond (prev
	      (setf (binding-across prev) (binding-across b))
	      bindings)
	     (t
	      (binding-across bindings))))))

;;; DELETE-VARIABLE  --  Public
;;;
;;; Make an editor variable no longer bound, fixing up the saved binding
;;; values as necessary.
;;;
(defun delete-variable (name &optional (kind :global) where)
  "Clear the definition of the editor variable $name in the place specified
   by $kind and $where.

   The following values for $kind are defined:

     :global
	 Return the global value.

     :mode
	 Return the value in the mode named $where.

     :buffer

	 Return the value in the buffer $where.

   Signal an error if the variable definition is already clear.  Invoke the
   hook *Delete Variable Hook* with the same arguments before the variable
   is deleted."
  (let* ((obj (get-variable-object name kind where))
	 (sname (variable-object-name obj)))
    (case kind
      (:buffer
       (let* ((values (buffer-var-values where))
	      (binding (find-binding name values)))
	 (invoke-hook ed::delete-variable-hook name :buffer where)
	 (delete-string sname (buffer-variables where))
	 (setf (buffer-var-values where) (delete-binding binding values))
	 (when (eq where *current-buffer*)
	   (setf (car (binding-cons binding)) (variable-object-down obj)))))
      (:mode
       (let* ((mode (get-mode-object where))
	      (values (mode-object-var-values mode))
	      (binding (find-binding name values)))
	 (invoke-hook ed::delete-variable-hook name :mode where)
	 (delete-string sname (mode-object-variables mode))
	 (if (memq mode (buffer-mode-objects *current-buffer*))
	     (let ((l (unwind-bindings mode)))
	       (setf (mode-object-var-values mode)
		     (delete-binding binding values))
	       (wind-bindings l))
	     (setf (mode-object-var-values mode)
		   (delete-binding binding values)))))
      (:global
       (invoke-hook ed::delete-variable-hook name :global nil)
       (delete-string sname *global-variable-names*)
       (let ((l (unwind-bindings nil)))
	 (setf (get name 'editor-variable-value) nil)
	 (wind-bindings l)))
      (t (error "Invalid variable kind: ~S" kind)))
    ()))


#[ The Current Environment

[ Scopes    ]
[ Shadowing ]
]#

#[ Scopes

The values of editor [variables], [key bindings] and [character attributes]
may depend on the [function:current-buffer] and the modes active in it.
There are three possible scopes for editor values:

  buffer local
     The value is present only if the buffer it is local
     to is the current-buffer.

  mode local
     The value is present only when the mode it is local to
     is active in the current-buffer.

  global
     The value is always present unless shadowed by a buffer or
     mode local value.
]#

#[ Shadowing

It is possible for there to be a conflict between different values for the
same thing in different scopes.  For example, there be might a global
binding for a given variable and also a local binding in the current
buffer.  Whenever there is a conflict shadowing occurs, permitting only one
of the values to be visible in the current environment.

The process of resolving such a conflict can be described as a search down
a list of places where the value might be defined, returning the first
value found.  The order for the search is as follows:

  * Local values in the current buffer.

  * Mode local values in the minor modes of the current buffer, in order
    from the highest precedence mode to the lowest precedence mode.  The
    order of minor modes with equal precedences is undefined.

  * Mode local values in the current buffer's major mode.

  * Global values.
]#
