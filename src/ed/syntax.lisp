;;; Syntax table routines.

(in-package "EDI")

(export '(character-attribute-name
	  defattribute character-attribute-documentation character-attribute
	  character-attribute-hooks character-attribute-p shadow-attribute
	  unshadow-attribute find-attribute reverse-find-attribute))

#[ Character Attributes

Character attributes provide a global database of information about characters.
This facility is similar to, but more general than, the syntax tables of
other editors such as Emacs.  For example, you should use character
attributes for commands that need information regarding whether a character is
whitespace.  Use character attributes for these reasons:

  * If this information is all in one place, then it is easy the change the
    behavior of the editor by changing the syntax table, much easier than it would
    be if character constants were wired into commands.

  * This centralization of information avoids needless duplication of effort.

  * The syntax table primitives are probably faster than anything that can be
    written above the primitive level.

Note that an essential part of the character attribute scheme is that
character attributes are global and are there for the user to change.
Information about characters which is internal to some set of commands (and
which the user should not know about) should not be maintained as a character
attribute.  For such uses various character searching abilities are provided by
the functions described in [Searching and Replacing (ext)].

{constant:syntax-char-code-limit}

[ Character Attribute Names           ]
[ Character Attribute Functions       ]
[ Character Attribute Hooks           ]
[ System Defined Character Attributes ]
]#


;;;; Character attribute caching.
;;;
;;; In order to permit the %SP-Find-Character-With-Attribute sub-primitive
;;; to be used for a fast implementation of find-attribute and
;;; reverse-find-attribute, there must be some way of translating
;;; attribute/test-function pairs into a attribute vector and a mask.
;;;
;;; What we do is maintain a eq-hash-cache of attribute/test-function
;;; pairs.  If the desired pair is not in the cache then we reclaim an old
;;; attribute bit in the bucket we hashed to and stuff it by calling the
;;; test function on the value of the attribute for all characters.

(defvar *character-attribute-cache* ()
  "This is the cache used to translate attribute/test-function pairs to
  attribute-vector/mask pairs for find-attribute and reverse-find-attribute.")

(eval-when (compile eval)
(defconstant character-attribute-cache-size 13
  "The number of buckets in the *character-attribute-cache*.")
(defconstant character-attribute-bucket-size 3
  "The number of bits to use in each bucket of the
  *character-attribute-cache*.")
); eval-when (compile eval)

;;; In addition, since a common pattern in code which uses find-attribute
;;; is to repeatedly call it with the same function and attribute, we
;;; remember the last attribute/test-function pair that was used, and check
;;; if it is the same pair beforehand, thus often avoiding the hastable
;;; lookup.
;;;
(defvar *last-find-attribute-attribute* ()
  "The attribute which we last did a find-attribute on.")
(defvar *last-find-attribute-function* ()
  "The last test-function used for find-attribute.")
(defvar *last-find-attribute-vector* ()
  "The %SP-Find-Character-With-Attribute vector corresponding to the last
  attribute/function pair used for find-attribute.")
(defvar *last-find-attribute-mask* ()
  "The the mask to use with *last-find-attribute-vector* to do a search
  for the last attribute/test-function pair.")
(defvar *last-find-attribute-end-wins* ()
  "The the value of End-Wins for the last attribute/test-function pair.")

(defvar *character-attributes* (make-hash-table :test #'eq)
  "A hash table which translates character attributes to their values.")
(defvar *last-character-attribute-requested* nil
  "The last character attribute which was asked for, Do Not Bind.")
(defvar *value-of-last-character-attribute-requested* nil
  "The value of the most recent character attribute, Do Not Bind.")

#[ Character Attribute Names

As for editor variables, character attributes have a user visible
string name, but are referred to in Lisp code as a symbol.  The string
name, which is typically composed of capitalized words separated by
spaces, is translated into a keyword by replacing all spaces with
hyphens and interning this string in the keyword package.  The
attribute named `Ada Syntax' would thus become :ada-syntax.

{variable:ed:*character-attribute-names*}
]#

(defvar *character-attribute-names* (make-string-table)
 "String table of character attribute names and their corresponding
  keywords.")

;;; Each bucket contains a list of character-attribute-bucket-size
;;; bit-descriptors.
;;;
(defstruct (bit-descriptor)
  function		      ; The test on the attribute.
  attribute		      ; The attribute this is a test of.
  (mask 0 :type fixnum)	      ; The mask for the corresponding bit.
  vector		      ; The vector the bit is in.
  end-wins)		      ; Is this test true of buffer ends?

;;;
;;; In a descriptor for an unused bit, the function is nil, preventing a
;;; hit.  Whenever we change the value of an attribute for some character,
;;; we need to flush the cache of any entries for that attribute.  Currently
;;; we do this by mapping down the list of all bit descriptors.  Note that
;;; we don't have to worry about GC, since this is just a hint.
;;;
(defvar *all-bit-descriptors* () "The list of all the bit descriptors.")


(eval-when (compile eval)
(defmacro allocate-bit (vec bit-num)
  `(progn
    (when (= ,bit-num 8)
      (setq ,bit-num 0  ,vec (make-array 256 :element-type '(mod 256))))
    (car (push (make-bit-descriptor
		:vector ,vec
		:mask (ash 1 (prog1 ,bit-num (incf ,bit-num))))
	       *all-bit-descriptors*)))))
;;;
(defun %init-syntax-table ()
  (let ((tab (make-array character-attribute-cache-size))
	(bit-num 8) vec)
    (setq *character-attribute-cache* tab)
    (dotimes (c character-attribute-cache-size)
      (setf (svref tab c)
	    (do ((i 0 (1+ i))
		 (res ()))
		((= i character-attribute-bucket-size) res)
	      (push (allocate-bit vec bit-num) res))))))


(eval-when (compile eval)
(defmacro hash-it (attribute function)
  `(abs (rem (logxor (ash (lisp::%sp-make-fixnum ,attribute) -3)
		     (lisp::%sp-make-fixnum ,function))
	     character-attribute-cache-size)))

;;; CACHED-ATTRIBUTE-LOOKUP  --  Internal
;;;
;;; Sets Vector and Mask such that they can be used as arguments to
;;; %sp-find-character-with-attribute to effect a search with attribute
;;; Attribute and test Function.  If the function and attribute are the
;;; same as the last ones then we just set them to that, otherwise we do
;;; the hash-cache lookup and update the *last-find-attribute-<mumble>*
;;;
(defmacro cached-attribute-lookup (attribute function vector mask end-wins)
  `(if (and (eq ,function *last-find-attribute-function*)
	    (eq ,attribute *last-find-attribute-attribute*))
       (setq ,vector *last-find-attribute-vector*
	     ,mask *last-find-attribute-mask*
	     ,end-wins *last-find-attribute-end-wins*)
       (let ((bit (svref *character-attribute-cache*
			 (hash-it ,attribute ,function))))
	 ,(do ((res `(multiple-value-setq (,vector ,mask ,end-wins)
		       (new-cache-attribute ,attribute ,function))
		    `(let ((b (car bit)))
		       (cond
			((and (eq (bit-descriptor-function b)
				  ,function)
			      (eq (bit-descriptor-attribute b)
				  ,attribute))
			 (setq ,vector (bit-descriptor-vector b)
			       ,mask (bit-descriptor-mask b)
			       ,end-wins (bit-descriptor-end-wins b)))
			(t
			 (setq bit (cdr bit)) ,res))))
	       (count 0 (1+ count)))
	      ((= count character-attribute-bucket-size) res))
	 (setq *last-find-attribute-attribute* ,attribute
	       *last-find-attribute-function* ,function
	       *last-find-attribute-vector* ,vector
	       *last-find-attribute-mask* ,mask
	       *last-find-attribute-end-wins* ,end-wins))))
); eval-when (compile eval)

;;; NEW-CACHE-ATTRIBUTE  --  Internal
;;;
;;; Pick out an old attribute to punt out of the cache and put in the new
;;; one.  We pick a bit off of the end of the bucket and pull it around to
;;; the beginning to get a degree of LRU'ness.
;;;
(defun new-cache-attribute (attribute function)
  (let* ((hash (hash-it attribute function))
	 (values (or (gethash attribute *character-attributes*)
		     (error "~S is not a defined character attribute."
			    attribute)))
	 (bucket (svref *character-attribute-cache* hash))
	 (bit (nthcdr (- character-attribute-bucket-size 2) bucket))
	 (end-wins (funcall function (attribute-descriptor-end-value values))))
    (shiftf bit (cdr bit) nil)
    (setf (svref *character-attribute-cache* hash) bit
	  (cdr bit) bucket  bit (car bit))
    (setf (bit-descriptor-attribute bit) attribute
	  (bit-descriptor-function bit) function
	  (bit-descriptor-end-wins bit) end-wins)
    (setq values (attribute-descriptor-vector values))
    (do ((mask (bit-descriptor-mask bit))
	 (fun (bit-descriptor-function bit))
	 (vec (bit-descriptor-vector bit))
	 (i 0 (1+ i)))
	((= i syntax-char-code-limit) (values vec mask end-wins))
      (declare (type (simple-array (mod 256)) vec))
      (if (funcall fun (aref (the simple-array values) i))
	  (setf (aref vec i) (logior (aref vec i) mask))
	  (setf (aref vec i) (logandc2 (aref vec i) mask))))))


(defun %print-attribute-descriptor (object stream depth)
  (declare (ignore depth))
  (format stream "#<Editor Attribute-Descriptor ~S>"
	  (attribute-descriptor-name object)))

#[ Character Attribute Functions

{function:ed:defattribute}
{function:ed:character-attribute-name}
{function:ed:character-attribute-documentation}
{function:ed:character-attribute}
{evariable:Character Attribute Hook}
{function:ed:character-attribute-p}
{function:ed:shadow-attribute}
{evariable:Shadow Attribute Hook}
{function:ed:unshadow-attribute}
{evariable:Unshadow Attribute Hook}
{function:ed:find-attribute}
{function:ed:reverse-find-attribute}
]#

;;; DEFATTRIBUTE  --  Public
;;;
;;; Make a new vector of some type and enter it in the table.
;;;
(defun defattribute (name documentation &optional (type '(mod 2))
			  (initial-value 0))
  "Define a new character attribute with simple-string $name.

   Character attribute operations take attribute arguments as a keyword
   whose name is $name uppercased with spaces replaced by hyphens.

   Set the description of the uses of the character attribute to
   $documentation.

   Set the type of the values of the character attribute to $type.  Values
   of a character attribute may be of any type which may be specified to
   `make-array'.

   Set the value which all characters will initially have for this
   attribute to $initial-value."
  (setq name (coerce name 'simple-string))
  (let* ((attribute (string-to-keyword name))
	 (new (make-attribute-descriptor
	       :vector (make-array syntax-char-code-limit
				   :element-type type
				   :initial-element initial-value)
	       :name name
	       :keyword attribute
	       :documentation documentation
	       :end-value initial-value)))
    (when (gethash attribute *character-attributes*)
      (warn "Character Attribute ~S is being redefined." name))
    (setf (getstring name *character-attribute-names*) attribute)
    (setf (gethash attribute *character-attributes*) new))
  name)

;;; WITH-ATTRIBUTE  --  Internal
;;;
;;; Bind obj to the attribute descriptor corresponding to symbol, giving
;;; error if it is not a defined attribute.
;;;
(eval-when (compile eval)
(defmacro with-attribute (symbol &body forms)
  `(let ((obj (gethash ,symbol *character-attributes*)))
     (unless obj
       (error "~S is not a defined character attribute." ,symbol))
     ,@forms))
); eval-when (compile eval)

(defun character-attribute-name (attribute)
  "Return the string-name of the character attribute $attribute."
  (with-attribute attribute
    (attribute-descriptor-name obj)))

(defun character-attribute-documentation (attribute)
  "Return the documentation for the character attribute $attribute."
  (with-attribute attribute
    (attribute-descriptor-documentation obj)))

#[ Character Attribute Hooks

It is often useful to use the character attribute mechanism as an abstract
interface to other information about characters which in fact is stored
elsewhere.  For example, some implementation of the editor might decide to
define a `Print Representation' attribute which controls how a character is
displayed on the screen.

To make this easy to do, each attribute has a list of hook functions
which are invoked with the attribute, character and new value whenever
the current value changes for any reason.

{function:ed:character-attribute-hooks}
]#

(defun character-attribute-hooks (attribute)
  "Return the current hook list for $attribute.

   This may be set with `setf'.

   The `add-hook' and `remove-hook' macros should be used to manipulate
   these lists."
  (with-attribute attribute
    (attribute-descriptor-hooks obj)))

(defun %set-character-attribute-hooks (attribute new-value)
  (with-attribute attribute
    (setf (attribute-descriptor-hooks obj) new-value)))

(proclaim '(special *last-character-attribute-requested*
		    *value-of-last-character-attribute-requested*))

;;; CHARACTER-ATTRIBUTE  --  Public
;;;
;;; Return the value of a character attribute for some character.
;;;
(proclaim '(inline character-attribute))
(defun character-attribute (attribute character)
  "Return the value of $attribute for $character.  Signal an error if the
   definition of attribute is ().

   `setf' will set a character's attributes.  This `setf' method invokes
   the functions in *Character Attribute Hook* on the attribute and
   character before it makes the change.

   If character is (), then the value of the attribute for the beginning or
   end of the buffer can be accessed or set.  The buffer beginning and end
   thus become a sort of fictitious character, which simplifies the use of
   character attributes in many cases."
  (if (and (eq attribute *last-character-attribute-requested*) character)
      (aref (the simple-array *value-of-last-character-attribute-requested*)
	    (syntax-char-code character))
      (sub-character-attribute attribute character)))
;;;
(defun sub-character-attribute (attribute character)
  (with-attribute attribute
    (setq *last-character-attribute-requested* attribute)
    (setq *value-of-last-character-attribute-requested*
	  (attribute-descriptor-vector obj))
    (if character
	(aref (the simple-array *value-of-last-character-attribute-requested*)
	      (syntax-char-code character))
	(attribute-descriptor-end-value obj))))

;;; CHARACTER-ATTRIBUTE-P
;;;
;;; Look up attribute in table.
;;;
(defun character-attribute-p (symbol)
  "Return true if $symbol is the name of a character attribute, else return
   ()."
  (not (null (gethash symbol *character-attributes*))))


;;; %SET-CHARACTER-ATTRIBUTE  --  Internal
;;;
;;; Set the value of a character attribute.
;;;
(defun %set-character-attribute (attribute character new-value)
  (with-attribute attribute
    (invoke-hook ed::character-attribute-hook attribute character new-value)
    (invoke-hook (attribute-descriptor-hooks obj) attribute character new-value)
    (cond
     ;;
     ;; Setting the value for a real character.
     (character
      (let ((value (attribute-descriptor-vector obj))
	    (code (syntax-char-code character)))
	(declare (type (simple-array *) value))
	(dolist (bit *all-bit-descriptors*)
	  (when (eq (bit-descriptor-attribute bit) attribute)
	    (let ((vec (bit-descriptor-vector bit)))
	      (declare (type (simple-array (mod 256)) vec))
	      (setf (aref vec code)
		    (if (funcall (bit-descriptor-function bit) new-value)
			(logior (bit-descriptor-mask bit) (aref vec code))
			(logandc1 (bit-descriptor-mask bit) (aref vec code)))))))
	(setf (aref value code) new-value)))
     ;;
     ;; Setting the magical end-value.
     (t
      (setf (attribute-descriptor-end-value obj) new-value)
      (dolist (bit *all-bit-descriptors*)
	(when (eq (bit-descriptor-attribute bit) attribute)
	  (setf (bit-descriptor-end-wins bit)
		(funcall (bit-descriptor-function bit) new-value))))
      new-value))))


(eval-when (compile eval)
;;; swap-one-attribute  --  Internal
;;;
;;; Install the mode-local values described by Vals for Attribute, whose
;;; representation vector is Value.
;;;
 (defmacro swap-one-attribute (attribute value vals hooks)
  `(progn
    ;; Fix up any cached attribute vectors.
    (dolist (bit *all-bit-descriptors*)
      (when (eq ,attribute (bit-descriptor-attribute bit))
	(let ((fun (bit-descriptor-function bit))
	      (vec (bit-descriptor-vector bit))
	      (mask (bit-descriptor-mask bit)))
	  (declare (type (simple-array (mod 256)) vec)
		   (fixnum mask))
	  (dolist (char ,vals)
	    (setf (aref vec (car char))
		  (if (funcall fun (cdr char))
		      (logior mask (aref vec (car char)))
		      (logandc1 mask (aref vec (car char)))))))))
    ;; Invoke the attribute-hook.
    (dolist (hook ,hooks)
      (dolist (char ,vals)
	(funcall hook ,attribute (code-char (car char)) (cdr char))))
    ;; Fix up the value vector.
    (dolist (char ,vals)
      (rotatef (aref ,value (car char)) (cdr char)))))
); eval-when (compile eval)

;;; SWAP-CHAR-ATTRIBUTES  --  Internal
;;;
;;; Swap the current values of character attributes and the ones specified
;;; by $mode.  This is used in `set-major-mode'.
;;;
(defun swap-char-attributes (mode)
  (dolist (attribute (mode-object-character-attributes mode))
    (let* ((obj (car attribute))
	   (sym (attribute-descriptor-keyword obj))
	   (value (attribute-descriptor-vector obj))
	   (hooks (attribute-descriptor-hooks obj)))
      (declare (simple-array value))
      (swap-one-attribute sym value (cdr attribute) hooks))))


(proclaim '(special *mode-names* *current-buffer*))

;;; SHADOW-ATTRIBUTE  --  Public
;;;
;;; Stick mode character attribute information in the mode object.
;;;
(defun shadow-attribute (attribute character value mode)
  "Establish $value as the value of $character's $attribute when in the
   $mode.

   $mode must be the name of a major mode.

   Invoke *Shadow Attribute Hook* with the same arguments.  If the value
   for an attribute is set while the value is shadowed, then only the
   shadowed value is affected (the global one is left the same)."
  (let ((desc (gethash attribute *character-attributes*))
	(obj (getstring mode *mode-names*)))
    (or desc
	(error "~S is not a defined Character Attribute." attribute))
    (or obj
	(error "~S is not a defined Mode." mode))
    (let* ((current (assq desc (mode-object-character-attributes obj)))
	   (code (syntax-char-code character))
	   (hooks (attribute-descriptor-hooks desc))
	   (vec (attribute-descriptor-vector desc))
	   (cons (cons code value)))
      (declare (simple-array vec))
      (if current
	  (let ((old (assq code (cdr current))))
	    (if old
		(setf (cdr old) value  cons old)
		(push cons (cdr current))))
	  (push (list desc cons)
		(mode-object-character-attributes obj)))
      (when (memq obj (buffer-mode-objects *current-buffer*))
	(let ((vals (list cons)))
	  (swap-one-attribute attribute vec vals hooks)))
      (invoke-hook ed::shadow-attribute-hook attribute character value mode)))
  attribute)

;;; UNSHADOW-ATTRIBUTE  --  Public
;;;
;;; Nuke a mode character attribute.
;;;
(defun unshadow-attribute (attribute character mode)
  "Clear the shadowing of the value of $attribute for $character in $mode.
   Call *Unshadow Attribute Hook* with the same arguments."
  (let ((desc (gethash attribute *character-attributes*))
	(obj (getstring mode *mode-names*)))
    (unless desc
      (error "~S is not a defined Character Attribute." attribute))
    (unless obj
      (error "~S is not a defined Mode." mode))
    (invoke-hook ed::shadow-attribute-hook mode attribute character)
    (let* ((value (attribute-descriptor-vector desc))
	   (hooks (attribute-descriptor-hooks desc))
	   (current (assq desc (mode-object-character-attributes obj)))
	   (char (assq (syntax-char-code character) (cdr current))))
      (declare (simple-array value))
      (unless char
	(error "Character Attribute ~S is not defined for character ~S ~
	       in Mode ~S." attribute character mode))
      (when (memq obj (buffer-mode-objects *current-buffer*))
	(let ((vals (list char)))
	  (swap-one-attribute attribute value vals hooks)))
      (setf (cdr current) (delete char (the list (cdr current))))))
  attribute)


;;; NOT-ZEROP, the default test function for find-attribute etc.
;;;
(defun not-zerop (n)
  (not (zerop n)))

;;; find-attribute  --  Public
;;;
;;; Do hairy cache lookup to find a find-character-with-attribute style
;;; vector that we can use to do the search.
;;;
(eval-when (compile eval)
(defmacro normal-find-attribute (line start result vector mask)
  `(let ((chars (line-chars ,line)))
     (setq ,result (%sp-find-character-with-attribute
		   chars ,start (strlen chars) ,vector ,mask))))
;;;
(defmacro cache-find-attribute (start result vector mask)
  `(let ((gap (- right-open-pos left-open-pos)))
     (declare (fixnum gap))
     (cond
      ((>= ,start left-open-pos)
       (setq ,result
	     (%sp-find-character-with-attribute
	      open-chars (+ ,start gap) line-cache-length ,vector ,mask))
       (when ,result (decf ,result gap)))
      ((setq ,result (%sp-find-character-with-attribute
		      open-chars ,start left-open-pos ,vector ,mask)))
      (t
       (setq ,result
	     (%sp-find-character-with-attribute
	      open-chars right-open-pos line-cache-length ,vector ,mask))
       (when ,result (decf ,result gap))))))
); eval-when (compile eval)
;;;
(defun find-attribute (mark attribute &optional (test #'not-zerop))
  "Find the next character with some value for character $attribute
   starting at $mark.

   Pass $test one argument, the value of $attribute for the character
   tested.  If the test succeeds, then modify $mark to point before the
   character which satisfied the test.

   If all characters fail the test, then return (), leaving mark the same.

   Expect $test to depend only on its argument."
  (let ((charpos (mark-charpos mark))
	(line (mark-line mark))
	(mask 0)
	vector end-wins)
    (declare (type (or (simple-array (mod 256)) null) vector) (fixnum mask)
	     (type (or fixnum null) charpos))
    (cached-attribute-lookup attribute test vector mask end-wins)
    (cond
     ((cond
       ((eq line open-line)
	(when (cache-find-attribute charpos charpos vector mask)
	  (setf (mark-charpos mark) charpos) mark))
       (t
	(when (normal-find-attribute line charpos charpos vector mask)
	  (setf (mark-charpos mark) charpos) mark))))
     ;; Newlines win and there is one.
     ((and (not (zerop (logand mask (aref vector (char-code #\newline)))))
	   (line-next line))
      (move-to-position mark (line-length line) line))
     ;; We can ignore newlines.
     (t
      (do (prev)
	  (())
	(setq prev line  line (line-next line))
	(cond
	 ((null line)
	  (if end-wins
	      (return (line-end mark prev))
	      (return nil)))
	 ((eq line open-line)
	  (when (cache-find-attribute 0 charpos vector mask)
	    (return (move-to-position mark charpos line))))
	 (t
	  (when (normal-find-attribute line 0 charpos vector mask)
	    (return (move-to-position mark charpos line))))))))))


;;; REVERSE-FIND-ATTRIBUTE  --  Public
;;;
;;; Line find-attribute, only goes backwards.
;;;
(eval-when (compile eval)
(defmacro rev-normal-find-attribute (line start result vector mask)
  `(let ((chars (line-chars ,line)))
     (setq ,result (%sp-reverse-find-character-with-attribute
		    chars 0 ,(or start '(strlen chars)) ,vector ,mask))))
;;;
(defmacro rev-cache-find-attribute (start result vector mask)
  `(let ((gap (- right-open-pos left-open-pos)))
     (declare (fixnum gap))
     (cond
      ,@(when start
	  `(((<= ,start left-open-pos)
	     (setq ,result
		   (%sp-reverse-find-character-with-attribute
		    open-chars 0 ,start ,vector ,mask)))))
      ((setq ,result (%sp-reverse-find-character-with-attribute
		      open-chars right-open-pos
		      ,(if start `(+ ,start gap) 'line-cache-length)
		      ,vector ,mask))
       (decf ,result gap))
      (t
       (setq ,result
	     (%sp-reverse-find-character-with-attribute
	      open-chars 0 left-open-pos ,vector ,mask))))))

); eval-when (compile eval)
;;;
(defun reverse-find-attribute (mark attribute &optional (test #'not-zerop))
  "Find the previous character with some value for character $attribute
   starting at $mark.

   Pass $test one argument, the value of $attribute for the character
   tested.  If the test succeeds, then modify $mark to point before the
   character which satisfied the test.

   If all characters fail the test, then return (), leaving mark the same.

   Expect $test to depend only on its argument."
  (let* ((charpos (mark-charpos mark))
	 (line (mark-line mark)) vector mask end-wins)
    (declare (type (or (simple-array (mod 256)) null) vector)
	     (type (or fixnum null) charpos))
    (cached-attribute-lookup attribute test vector mask end-wins)
    (cond
     ((cond
       ((eq line open-line)
	(when (rev-cache-find-attribute charpos charpos vector mask)
	  (setf (mark-charpos mark) (1+ charpos)) mark))
       (t
	(when (rev-normal-find-attribute line charpos charpos vector mask)
	  (setf (mark-charpos mark) (1+ charpos)) mark))))
     ;; Newlines win and there is one.
     ((and (line-previous line)
	   (not (zerop (logand mask (aref vector (char-code #\newline))))))
      (move-to-position mark 0 line))
     (t
      (do (next)
	  (())
	(setq next line  line (line-previous line))
	(cond
	 ((null line)
	  (if end-wins
	      (return (line-start mark next))
	      (return nil)))
	 ((eq line open-line)
	  (when (rev-cache-find-attribute nil charpos vector mask)
	    (return (move-to-position mark (1+ charpos) line))))
	 (t
	  (when (rev-normal-find-attribute line nil charpos vector mask)
	    (return (move-to-position mark (1+ charpos) line))))))))))
