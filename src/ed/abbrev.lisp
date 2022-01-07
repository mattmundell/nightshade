;;; Word abbreviation mode.

(in-package "ED")

#[ Word Abbreviation

Word abbreviation provides a way to speed the typing of frequently used words
and phrases.  When in `Abbrev' mode, typing a word delimiter causes the
previous word to be replaced with its expansion if there is one currently
defined.  The expansion for an abbrev may be any string, so this mode can be
used for abbreviating programming language constructs and other more obscure
uses.  For example, `Abbrev' mode can be used to automatically correct
common spelling mistakes and to enforce consistent capitalization of
identifiers in programs.

Abbrev is an abbreviation for abbreviation, which is used for
historical reasons.  Obviously the original writer of `Abbrev' mode hated
to type long words and could hardly use `Abbreviation' mode while writing
`Abbrev' mode.

A word abbrev can be either global or local to a major mode.  A global word
abbrev is defined no matter what the current major mode is, while a mode word
abbrev is only defined when its mode is the major mode in the current buffer.
Mode word abbrevs can be used to prevent abbrev expansion in inappropriate
contexts.

[ Basic Abbrev Commands ]
[ Word Abbrev Files     ]
[ Listing Word Abbrevs  ]
[ Editing Word Abbrevs  ]
[ Deleting Word Abbrevs ]
]#

(defmode "Abbrev" :major-p nil :transparent-p t :precedence 2.0
  :documentation
  "`Abbrev' mode must be on for the automatic expansion of word abbrevs to
   occur, but the abbreviation commands are bound globally and may be used
   at any time.")

(defvar *Global-Abbrev-Table* (make-hash-table :test #'equal)
  "Hash table holding global abbrev definitions.")

(defevar "Abbrev Pathname Defaults"
  "The name of the last Abbrev file written."
  :value (pathname "abbrev.defns"))

(defvar *new-abbrevs* ()
 "A list of abbrevs (and their definitions and modes) changed since
  saving.")


#[ Basic Abbrev Commands

{mode:Abbrev}
{command:Abbrev Expand Only}
{command:Inverse Add Global Word Abbrev}
{command:Inverse Add Mode Word Abbrev}
{command:Make Word Abbrev}
{command:Add Global Word Abbrev}
{command:Add Mode Word Abbrev}
{command:Word Abbrev Prefix Mark}
{command:Unexpand Last Word}
]#


(defcommand "Inverse Add Mode Word Abbrev" ()
  "Prompt for a string and make it the word abbrev expansion for the word
   before the point for the current major mode."
  (let ((word (prev-word 1 (current-point)))
	(mode (buffer-major-mode (current-buffer))))
    (make-word-abbrev-command nil word nil mode)))

(defcommand "Add Mode Word Abbrev" (p)
  "Define a prompted word to be a word abbreviation in the current major
   mode.  The prefix argument determines which text is used as the
   expansion:

     () prefix argument
	The word before the point is used as the expansion of
	the abbreviation.

     zero prefix argument
	The text in the region is used as the expansion of the
	abbreviation.

     positive prefix argument
	That many words before the point are made the
	expansion of the abbreviation.

     negative prefix argument
	Do the same thing as
	`Delete Global Word Abbrev' instead of defining an abbreviation."
  (if (and p (minusp p))
      (delete-mode-word-abbrev-command)
      (let* ((val (if (eql p 0)
		      (region-to-string (current-region nil))
		      (prev-word (or p 1) (current-point))))
	     (mode (buffer-major-mode (current-buffer))))
	(make-word-abbrev-command nil nil val mode))))

(defcommand "Inverse Add Global Word Abbrev" ()
  "Prompt for a string and make it the global word abbrev expansion for the
   word before the point."
  (let ((word (prev-word 1 (current-point))))
    (make-word-abbrev-command nil word nil "Global")))

(defcommand "Add Global Word Abbrev" (p)
  "Define a prompted word to be a global word abbreviation.  The prefix
   argument determines which text is used as the expansion:

     () prefix argument
	The word before the point is used as the expansion of
	the abbreviation.

     zero prefix argument
	The text in the region is used as the expansion of the
	abbreviation.

     positive prefix argument
	That many words before the point are made the
	expansion of the abbreviation.

     negative prefix argument
	Do the same thing as
	`Delete Global Word Abbrev' instead of defining an abbreviation."
  (if (and p (minusp p))
      (delete-global-word-abbrev-command)
      (let ((val (if (eql p 0)
		     (region-to-string (current-region nil))
		     (prev-word (or p 1) (current-point)))))
	(make-word-abbrev-command nil nil val "Global"))))


;;;; Defining Abbrevs

(defvar *global-abbrev-string-table*
  (make-string-table :initial-contents '(("Global" . nil))))

(defcommand "Make Word Abbrev" (p abbrev expansion mode)
  "Define an arbitrary word abbreviation.  Prompt for abbrev, expansion,
   and mode.  If the mode \"Global\" is specified, then make a global
   abbrev."
  "Make Abbrev a word abbreviation for Expansion when in Mode.  If mode is
   \"Global\" then make a global abbrev."
  (declare (ignore p))
  (or mode
      (setq mode
	    (prompt-for-keyword
	     (list *mode-names* *global-abbrev-string-table*)
	     :prompt "Mode of abbrev to add: "
	     :default "Global"
	     :help
	     "Type the mode of the Abbrev you want to add, or confirm for Global.")))
  (let ((globalp (string-equal mode "Global")))
    (or globalp (mode-major-p mode)
	(editor-error "~A is not a major mode." mode))
    (or abbrev
	(setq abbrev
	      (prompt-for-string
	       :trim t
	       :prompt
	       (list "~A abbreviation~@[ of ~S~]: " mode expansion)
	       :help
	       (list "Define a ~A word abbrev." mode))))
    (when (zerop (length abbrev))
      (editor-error "Abbreviation must be at least one character long."))
    (or (every #'(lambda (ch)
		   (zerop (character-attribute :word-delimiter ch)))
	       (the simple-string abbrev))
	(editor-error "Word Abbrevs must be a single word."))
    (or expansion
	(setq expansion
	      (prompt-for-string
	       :prompt (list "~A expansion for ~S: " mode abbrev)
	       :help (list "Define the ~A expansion of ~S." mode abbrev))))
    (setq abbrev (string-downcase abbrev))
    (let* ((table (cond (globalp *global-abbrev-table*)
			((editor-bound-p 'Mode-Abbrev-Table :mode mode)
			 (variable-value 'Mode-Abbrev-Table :mode mode))
			(t
			 (let ((new (make-hash-table :test #'equal)))
			   (defevar "Mode Abbrev Table"
			     "Hash Table of Mode Abbrevs"
			     :value new :mode mode)
			   new))))
	   (old (gethash abbrev table)))
      (when (or (not old)
		(prompt-for-y-or-n
		 :prompt
		 (list "Current ~A definition of ~S is ~S.~%Redefine?"
		       mode abbrev old)
		 :default t
		 :help (list "Redefine the expansion of ~S." abbrev)))
	(setf (gethash abbrev table) expansion)
	(push (list abbrev expansion (if globalp nil mode))
	      *new-abbrevs*)))))


(defcommand "Abbrev Expand Only" ()
  "Expands the word before point into its abbrev definition (if it has
   one).  If the word before the point is a defined word abbrev, then
   replace it with its expansion.  The replacement is done using the same
   case-preserving heuristic as is used by `Query Replace'.  This command
   is globally bound to M-Space so that abbrevs can be expanded when
   `Abbrev' mode is off.  An undesirable expansion may be inhibited by
   using C-q to insert the delimiter."
  (let* ((word (prev-word 1 (current-point)))
	 (glob (gethash (string-downcase word) *global-abbrev-table*))
	 (mode (if (editor-bound-p 'Mode-Abbrev-Table)
		   (gethash (string-downcase word)
			    (value Mode-Abbrev-Table))))
	 (end-word (reverse-find-attribute (copy-mark (current-point)
						      :right-inserting)
					   :word-delimiter #'zerop))
	 (result (if mode mode glob)))
    (when (or mode glob)
      (delete-characters end-word (- (length word)))
      (cond ((equal word (string-capitalize word))
	     (setq result (string-capitalize result)))
	    ((equal word (string-upcase word))
	     (setq result (string-upcase result))))
      (insert-string end-word result)
      (or (editor-bound-p 'last-expanded)
	  (defevar "last expanded"
	    "Holds a mark, the last expanded abbrev, and its expansion in a list."
	    :buffer (current-buffer)))
      (setf (value last-expanded)
	    (list (copy-mark (current-point) :right-inserting)
		  word result)))
    (delete-mark end-word))
  (when (and (editor-bound-p 'prefix-mark)
	     (value prefix-mark))
    (delete-characters (value prefix-mark) 1)
    (delete-mark (value prefix-mark))
    (setf (value prefix-mark) nil)))

(defun prev-word (n mark)
  "Return the N words immediately before MARK."
  (let* ((mark-1 (reverse-find-attribute (copy-mark mark :temporary)
					 :word-delimiter #'zerop))
	 (mark-2 (copy-mark mark-1)))
    (dotimes (x n (region-to-string (region mark-2 mark-1)))
      (reverse-find-attribute (mark-before mark-2) :word-delimiter))))

;;; When "Abbrev Expand Only" expands the abbrev (because #\- is an
;;; expander) it will see that prefix-mark is true, and will delete the #\-
;;; immediately after prefix-mark.

(defcommand "Word Abbrev Prefix Mark" ()
  "Mark a prefix to be glued to an abbrev following.

   This allows `Abbrev Expand Only' to recognize abbreviations when
   they have prefixes attached.  First type the prefix, then use this
   command.  A hyphen (-) will be inserted in the buffer.  Now type the
   abbreviation and the word delimiter.  `Abbrev Expand Only' will expand
   the abbreviation and remove the hyphen.

   Note that there is no need for a suffixing command, since `Abbrev Expand
   Only' may be used explicitly by typing M-Space."
  (or (editor-bound-p 'prefix-mark)
      (defevar "prefix mark"
	"Holds a mark (or not) pointing to the current Prefix Mark."
	:buffer (current-buffer)))
  (when (value prefix-mark)
    (delete-mark (value prefix-mark)))
  (setf (value prefix-mark) (copy-mark (current-point) :right-inserting))
  (insert-character (value prefix-mark) #\-))


(defcommand "Unexpand Last Word" ()
  "Revert the last abbrev expansion, or revert `Unexpand Last Word'.  Only
   one abbrev may be reverted."
  (if (if (editor-bound-p 'last-expanded)
	  (fi (value last-expanded))
	  t)
      (editor-error "Nothing to Undo."))
  (let ((mark (car (value last-expanded)))
	(word1 (second (value last-expanded)))
	(word2 (third (value last-expanded))))
    (or (string= word2
		 (region-to-string
		  (region (character-offset (copy-mark mark :temporary)
					    (- (length word2)))
			  mark)))
	(editor-error "The last expanded Abbrev has been altered in the text."))
    (delete-characters mark (- (length word2)))
    (insert-string mark word1)
    (character-offset mark (length word1))
    (setf (value last-expanded) (list mark word2 word1))))


;;;; Deleting.

#[ Deleting Word Abbrevs

The user may delete word abbrevs either individually or collectively.
Individual abbrev deletion neutralizes single abbrevs which have outlived
their usefulness; collective deletion provides a clean slate from which to
initiate abbrev definitions.

{command:Delete All Word Abbrevs}
{command:Delete Global Word Abbrev}
{command:Delete Mode Word Abbrev}
]#

(defcommand "Delete Mode Word Abbrev"
	    (&optional p abbrev
		       (mode (buffer-major-mode (current-buffer))))
  "Prompt for a word abbrev and deletes the mode expansion in the current
   mode.  If called with a prefix argument, delete all word abbrevs defined
   in the current mode."
  "Delete Abbrev in Mode, or all abbrevs in Mode if P is true."
  (let ((boundp (editor-bound-p 'Mode-Abbrev-Table :mode mode)))
    (if p
	(when boundp
	  (delete-variable 'Mode-Abbrev-Table :mode mode))
	(let ((down
	       (string-downcase
		(or abbrev
		    (prompt-for-string
		     :prompt (list "~A abbrev to delete: " mode)
		     :help
 (list "Give the name of a ~A mode word abbrev to delete." mode)
		     :trim t))))
	      (table (and boundp (variable-value 'mode-abbrev-table :mode mode))))
	  (or (and table (gethash down table))
	      (editor-error "~S is not the name of an abbrev in ~A mode."
			    down mode))
	  (remhash down table)))))

(defcommand "Delete Global Word Abbrev" (p abbrev)
  "Prompt for a word abbrev and delete the global expansion.  If called
   with a prefix argument, delete all global abbrevs."
  "Delete the global word abbreviation named Abbrev.  If P is true, delete
   all global abbrevs."
  (if p
      (setq *global-abbrev-table* (make-hash-table :test #'equal))
      (let ((down
	     (string-downcase
	      (or abbrev
		  (prompt-for-string
		   :prompt "Global abbrev to delete: "
		   :help "Give the name of a global word abbrev to delete."
		   :trim t)))))
	(or (gethash down *global-abbrev-table*)
	    (editor-error "~S is not the name of a global word abbrev." down))
	(remhash down *global-abbrev-table*))))

(defcommand "Delete All Word Abbrevs" ()
  "Deletes all word abbrevs which are currently defined."
  (delete-global-word-abbrev-command 1)
  (delete-mode-word-abbrev-command 1))


;;;; Abbrev I/O

#[ Listing Word Abbrevs

{command:List Word Abbrevs}
{command:Word Abbrev Apropos}
]#

(defcommand "List Word Abbrevs" (p)
  "List each of the currently defined Word Abbrevs, with its mode and
   expansion."
  (word-abbrev-apropos-command p ""))

(defcommand "Word Abbrev Apropos" (p search-string)
  "List all of the currently defined Word Abbrevs which contain a given
   string in their abbrev. definition, or mode."
  (declare (ignore p))
  (or search-string
      (setq search-string
	    (string-downcase
	     (prompt-for-string
	      :prompt "Apropos string: "
	      :help "The string to search word abbrevs and definitions for."))))
  (multiple-value-bind (count mode-tables) (count-abbrevs)
    (with-pop-up-display (s :height (min (1+ count) 30))
      (or (zerop (hash-table-count *global-abbrev-table*))
	  (maphash #'(lambda (key val)
		       (when (or (search search-string (string-downcase key))
				 (search search-string (string-downcase val)))
			 (write-abbrev key val nil s t)))
		   *global-abbrev-table*))
      (dolist (modename mode-tables)
	(let ((table (variable-value 'Mode-Abbrev-Table :mode modename)))
	  (if (search search-string (string-downcase modename))
	      (maphash #'(lambda (key val)
			   (write-abbrev key val modename s t))
		       table)
	      (maphash #'(lambda (key val)
			   (when (or (search search-string (string-downcase key))
				     (search search-string (string-downcase val)))
			     (write-abbrev key val modename s t)))
		       table))))
      (terpri s))))

(defun count-abbrevs ()
  (let* ((count (hash-table-count *global-abbrev-table*))
	 (mode-tables nil))
    (do-strings (which x *mode-names*)
      (declare (ignore x))
      (when (editor-bound-p 'Mode-Abbrev-Table :mode which)
	(let ((table-count (hash-table-count (variable-value 'Mode-Abbrev-Table
							     :mode which))))
	  (unless (zerop table-count)
	    (incf count table-count)
	    (push which mode-tables)))))
    (values count mode-tables)))


#[ Editing Word Abbrevs

Word abbrev definition lists are edited by editing the text representation
of the definitions.  Word abbrev files may be edited directly, like any other
text file.  The set of abbrevs currently defined in the editor may be edited
using the commands described in this section.

The text representation of a word abbrev is fairly simple.  Each definition
begins at the beginning of a line.  Each line has three fields which are
separated by ASCII tab characters.  The fields are the abbreviation, the
mode of the abbreviation and the expansion.  The mode is represented as the
mode name inside of parentheses.  If the abbrev is global, then the mode
field is empty.  The expansion is represented as a quoted string since it
may contain any character.  The string is quoted with double-quotes;
double-quotes in the expansion are represented by doubled double-quotes.
The expansion may contain newline characters, in which case the definition
will take up more than one line.

{command:Edit Word Abbrevs}
{command:Insert Word Abbrevs}
{command:Define Word Abbrevs}
]#

(defcommand "Edit Word Abbrevs" ()
  "Insert the current word abbrev definitions into the `Edit Word Abbrevs'
   buffer and then enter a recursive edit on the buffer.  When the
   recursive edit is exited, make the definitions in the buffer the new
   current abbrev definitions."
  (when (getstring "Edit Word Abbrevs" *buffer-names*)
    (delete-buffer (getstring "Edit Word Abbrevs" *buffer-names*)))
  (let ((old-buf (current-buffer))
	(new-buf (make-buffer "Edit Word Abbrevs")))
    (change-to-buffer new-buf)
    (unwind-protect
      (progn
       (insert-word-abbrevs-command)
       (do-recursive-edit)
       (or (equal #\newline (previous-character (buffer-end (current-point))))
	   (insert-character (current-point) #\newline))
       (delete-all-word-abbrevs-command)
       (define-word-abbrevs-command))
      (progn
       (change-to-buffer old-buf)
       (delete-buffer new-buf)))))

(defcommand "Insert Word Abbrevs" ()
  "Insert at the point the text representation of the currently defined
   word abbrevs."
  (multiple-value-bind (x mode-tables)
		       (count-abbrevs)
    (declare (ignore x))
    (with-output-to-mark (stream (current-point) :full)
      (maphash #'(lambda (key val)
		   (write-abbrev key val nil stream))
	       *global-abbrev-table*)

      (dolist (mode mode-tables)
	(let ((modename (if (listp mode) (car mode) mode)))
	  (maphash #'(lambda (key val)
		       (write-abbrev key val modename stream))
		   (variable-value 'Mode-Abbrev-Table :mode modename)))))))

(defcommand "Define Word Abbrevs" ()
  "Interpret the text of the current buffer as a word abbrev definition
   list (as produced by `Insert Word Abbrevs'), adding all the definitions
   to those currently defined."
  (with-input-from-region (file (buffer-region (current-buffer)))
    (read-abbrevs file)))


#[ Word Abbrev Files

A word abbrev file is a file which holds word abbrev definitions.  Word abbrev
files allow abbrevs to be saved so that they may be used across many editing
sessions.

{evariable:Abbrev Pathname Defaults}
{command:Read Word Abbrev File}
{command:Write Word Abbrev File}
{command:Append to Word Abbrev File}
]#

(defcommand "Read Word Abbrev File" (p filename)
  "Read in a file of previously defined abbrev definitions.  Add the
   definitions to those currently defined, replacing any that exist
   already.

   Ignores all lines less than 4 characters, i.e. blankspace or errors. That is
   the minimum number of characters possible to define an abbrev.  It thinks the
   current abbrev \"wraps\" if there is no #\" at the end of the line or there are
   two #\"s at the end of the line (unless that is the entire definition string,
   i.e, a null-abbrev).

   The format of the Abbrev files is

   ABBREV<tab><tab>\"ABBREV DEFINITION\"

   for Global Abbrevs, and

   ABBREV<tab>(MODE)<tab>\"ABBREV DEFINITION\"

   for Modal Abbrevs.

   Double-quotes contained within the abbrev definition are doubled.  If the first
   line of an abbrev definition is not closed by a single double-quote, then
   the subsequent lines are read in until a single double-quote is found."
  "Reads in a file of previously defined abbrev definitions."
  (declare (ignore p))
  (setf (value abbrev-pathname-defaults)
	(if filename
	    filename
	    (prompt-for-file
	     :prompt "Name of abbrev file: "
	     :help "The name of the abbrev file to load."
	     :default (value abbrev-pathname-defaults)
	     :must-exist nil)))
  (with-open-file (file (value abbrev-pathname-defaults) :direction :input
			:element-type 'base-char :if-does-not-exist :error)
    (read-abbrevs file)))

(defun read-abbrevs (file)
  "Do the actual defining of abbrevs from a given stream, expecting tabs
   and doubled double-quotes."
  (do ((line (read-line file nil nil)
	     (read-line file nil nil)))
      ((null line))
    (unless (< (length line) 4)
      (let* ((tab (position #\tab line))
	     (tab2 (position #\tab line :start (1+ tab)))
	     (abbrev (subseq line 0 tab))
	     (modename (subseq line (1+ tab) tab2))
	     (expansion (do* ((last (1+ (position #\" line))
				    (if found (min len (1+ found)) 0))
			      (len (length line))
			      (found (if (position #\" line :start last)
					 (1+ (position #\" line :start last)))
				     (if (position #\" line :start last)
					 (1+ (position #\" line :start last))))
			      (expansion (subseq line last (if found found len))
					 (concatenate 'simple-string expansion
						      (subseq line last
							      (if found found
								  len)))))
			     ((and (or (null found) (= found len))
				   (equal #\" (char line (1- len)))
				   (or (not (equal #\" (char line (- len 2))))
				       (= (- len 3) tab2)))
			      (subseq expansion 0 (1- (length expansion))))

			  (when (null found)
			    (setq line (read-line file nil nil)
				  last 0
				  len (length line)
				  found (if (position #\" line)
					    (1+ (position #\" line)))
				  expansion (format nil "~A~%~A" expansion
						    (subseq line 0 (if found
								       found
								       0))))))))

	(cond ((equal modename "")
	       (setf (gethash abbrev *global-abbrev-table*)
		     expansion))
	      (t (setq modename (subseq modename 1 (1- (length modename))))
		 (or (editor-bound-p 'Mode-Abbrev-Table
					  :mode modename)
		     (defevar "Mode Abbrev Table"
		       "Hash Table of Mode Abbrevs"
		       :value (make-hash-table :test #'equal)
		       :mode modename))
		 (setf (gethash abbrev (variable-value
					'Mode-Abbrev-Table :mode modename))
		       expansion)))))))

(defcommand "Write Word Abbrev File" (p filename)
  "Save the currently defined Abbrevs to a prompted file."
  (declare (ignore p))
  (or filename
      (setq filename
	    (prompt-for-file
	     :prompt "Write abbrevs to file: "
	     :default (value abbrev-pathname-defaults)
	     :help "Name of the file to write current abbrevs to."
	     :must-exist nil)))
  (with-open-file (file filename :direction :output
			:element-type 'base-char :if-exists :supersede
			:if-does-not-exist :create)
    (multiple-value-bind (x mode-tables) (count-abbrevs)
      (declare (ignore x))
      (maphash #'(lambda (key val)
		   (write-abbrev key val nil file))
	       *global-abbrev-table*)

      (dolist (modename mode-tables)
	(let ((mode (if (listp modename) (car modename) modename)))
	  (maphash #'(lambda (key val)
		       (write-abbrev key val mode file))
		   (variable-value 'Mode-Abbrev-Table :mode mode))))))
  (let ((tn (truename filename)))
    (setf (value abbrev-pathname-defaults) tn)
    (message "~A written." (namestring tn))))

(defcommand "Append to Word Abbrev File" (p filename)
  "Append Abbrevs defined or redefined since the last save to a prompted
   file.  Definitions made by reading word abbrev files are not
   considered."
  (declare (ignore p))
  (cond
   (*new-abbrevs*
    (or filename
	(setq filename
	      (prompt-for-file
	       :prompt
	       "Append incremental abbrevs to file: "
	       :default (value abbrev-pathname-defaults)
	       :must-exist nil
	       :help "Filename to append recently defined Abbrevs to.")))
    (write-incremental :append filename))
   (t
    (message "No Abbrev definitions have been changed since the last write."))))

(defun write-incremental (mode filename)
  (with-open-file (file filename :direction :output
			:element-type 'base-char
			:if-exists mode :if-does-not-exist :create)
    (dolist (def *new-abbrevs*)
      (let ((abb (car def))
	    (val (second def))
	    (mode (third def)))
	(write-abbrev abb val mode file))))
  (let ((tn (truename filename)))
    (setq *new-abbrevs* nil)
    (setf (value abbrev-pathname-defaults) tn)
    (message "~A written." (namestring tn))))

(defun write-abbrev (abbrev expansion modename file &optional flag)
  "Given an Abbrev, expansion, mode (nil for Global), and stream, this
   function writes to the stream with doubled double-quotes and stuff.  If
   the flag is true, then the output is in a pretty format (like `List Word
   Abbrevs' uses), otherwise output is in tabbed format (like `Write Word
   Abbrev File' uses)."
  (if flag
      (if modename
	  (format file "~5t~A~20t(~A)~35t\"" abbrev modename); pretty format
	  (format file "~5t~A~35t\"" abbrev))                ; pretty format
      (cond (modename
	     (write-string abbrev file)
	     (write-char #\tab file)
	     (format file "(~A)" modename)                   ; "~A<tab>(~A)<tab>\""
	     (write-char #\tab file)
	     (write-char #\" file))
	    (t
	     (write-string abbrev file)
	     (write-char #\tab file)                         ; "~A<tab><tab>\""
	     (write-char #\tab file)
	     (write-char #\" file))))
  (do* ((prev 0 found)
	(found (position #\" expansion)
	       (position #\" expansion :start found)))
       ((not found)
	(write-string expansion file :start prev)
	(write-char #\" file)
	(terpri file))
    (incf found)
    (write-string expansion file :start prev :end found)
    (write-char #\" file)))

(defconstant abbrevs-save-name "abbrevs")

(defun save-abbrevs ()
  (write-word-abbrev-file-command
   ()
   (config:config-pathname abbrevs-save-name)))

(add-hook exit-hook 'save-abbrevs)

(after-editor-initializations
 (if (config:probe-config-file abbrevs-save-name)
     (read-word-abbrev-file-command
      ()
      (config:config-pathname abbrevs-save-name))))
