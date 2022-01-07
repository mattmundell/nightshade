;;; Machine/filesystem independent pathname functions.

(in-package "LISP")

;; FIX pathname-p?
(export '(pathname pathnamep relativep absolutep circular-p
	  logical-pathname logical-pathname-p
	  remote-pathname-p remote-pathname-host remote-pathname-local
	  parse-namestring merge-pathnames make-pathname
	  pathname-host pathname-device pathname-directory pathname-name
	  pathname-type pathname-version namestring file-namestring
	  directory-namestring host-namestring enough-namestring
	  wild-pathname-p common-prefix pathname-match-p translate-pathname
	  translate-logical-pathname logical-pathname-translations
	  load-logical-pathname-translations *default-pathname-defaults*))

(in-package "EXTENSIONS")
(export '(search-list search-list-defined-p clear-search-list
	  enumerate-search-list))

(in-package "LISP")


#[ Pathnames

[ Unix Pathnames          ]
[ Wildcard Pathnames      ]
[ Logical Pathnames       ]
[ Search Lists            ]
[ Predefined Search-Lists ]
[ Search-List Operations  ]
[ Search List Example     ]
]#

#[ Unix Pathnames

Unix pathnames are always parsed with a unix-host object as the host and
() as the device.  The last two dots (.) in the namestring mark
the type and version, however if the first character is a dot, it is considered
part of the name.  If the last character is a dot, then the pathname has the
empty-string as its type.  The type defaults to () and the version
defaults to :newest.

    (defun parse (x)
      (values (pathname-name x) (pathname-type x) (pathname-version x)))

    (parse "foo")         => "foo", NIL, :NEWEST
    (parse "foo.bar")     => "foo", "bar", :NEWEST
    (parse ".foo")        => ".foo", NIL, :NEWEST
    (parse ".foo.bar")    => ".foo", "bar", :NEWEST
    (parse "..")          => ".", "", :NEWEST
    (parse "foo.")        => "foo", "", :NEWEST
    (parse "foo.bar.1")   => "foo", "bar", 1
    (parse "foo.bar.baz") => "foo.bar", "baz", :NEWEST

The directory of pathnames beginning with a slash (or a search-list,
[Search Lists]) starts with :absolute, others start with :relative.  The ..
directory is parsed as :up; there is no namestring for :back.

    (pathname-directory "/usr/foo/bar.baz") => (:ABSOLUTE "usr" "foo")
    (pathname-directory "../foo/bar.baz")   => (:RELATIVE :UP "foo")
]#

#[ Wildcard Pathnames

Wildcards are supported in Unix pathnames.  If '*' is specified for a part
of a pathname, that is parsed as :wild.  "**" can be used as a directory
name to indicate :wild-inferiors.  Filesystem operations treat
:wild-inferiors the same as :wild, but pathname pattern matching (e.g. for
translation of [logical pathnames]) matches any number of directory parts
with "**" (as in [Wildcard Matching].)

'*' embedded in a pathname part matches any number of characters.
Similarly, '?' matches exactly one character, and `[a,b]' matches the
characters 'a' or 'b'.  These pathname parts are parsed as pattern objects.

Backslash can be used as an escape character in namestring parsing to
prevent the next character from being treated as a wildcard.  Note that if
typed in a string constant, the backslash must be doubled, since the string
reader also uses backslash as a quote:

    (pathname-name "foo\\*bar") => "foo*bar"
]#

#[ Logical Pathnames

If a namestring begins with the name of a defined logical pathname host
followed by a colon, then it will be parsed as a logical pathname.  Both
'*' and "**" wildcards are implemented.
\findexed{load-logical-pathname-defaults} on \var{name} looks for a logical
host definition file in library:name.translations (Note that library:
designates the search list initialized to the Nightshade "lib/" directory,
not a logical pathname.)  The format of the file is a single list of
two-lists of the from and to patterns:

    (("foo;*.text" "/usr/ram/foo/*.txt")
     ("foo;*.lisp" "/usr/ram/foo/*.l"))
]#

#[ Search Lists

Search lists are an extension to Common Lisp pathnames.  They serve a function
somewhat similar to Common Lisp logical pathnames, but work more like Unix PATH
variables.  Search lists are used for two purposes:

  * They provide a convenient shorthand for commonly used directory names,
    and

  * They allow the abstract (directory structure independent) specification
    of file locations in program pathname constants (similar to logical
    pathnames.)

Each search list has an associated list of directories (represented as
pathnames with no name or type component.)  The namestring for any relative
pathname may be prefixed with slist:, indicating that the pathname is
relative to the search list slist (instead of to the current working
directory.)  Once qualified with a search list, the pathname is no longer
considered to be relative.

When a search list qualified pathname is passed to a file-system operation
such as `open', `load' or `truename', each directory in the search list is
successively used as the root of the pathname until the file is located.
When a file is written to a search list directory, the file is always
written to the first directory in the list.
]#

#[ Predefined Search-Lists

These search-lists are initialized from the Unix environment or when Lisp was
built:

  % home: and :

    The user's home directory.

  % library:

    The lib/ directory (NIGHTSHADELIB environment variable.)

  % path:

    The Unix command path (PATH environment variable.)

  % target:

    The root of the tree where the system was compiled.

It can be useful to redefine these search-lists, for example, library: can
be augmented to allow logical pathname translations to be located, and
target: can be redefined to point to the local location of the system
sources.
]#

#[ Search-List Operations

These operations define and access search-list definitions.  A search-list name
may be parsed into a pathname before the search-list is actually defined, but
the search-list must be defined before it can actually be used in a filesystem
operation.

{function:ext:search-list}
{function:ext:search-list-defined-p}
{function:ext:clear-search-list}
{function:ext:enumerate-search-list}
]#

#[ Search List Example

The search list code: could be defined as follows:

    (setf (ext:search-list "code:") '("/usr/lisp/code/"))

It is now possible to use code: as an abbreviation for the directory
"/usr/lisp/code/" in all file operations.  For example, code:eval.lisp
refers to the file /usr/lisp/code/eval.lisp.

The function search-list produces the value of a search-list name, as
follows:

    (ext:search-list NAME)

Where NAME is the name of a search list as described above.  For example,
calling ext:search-list on code: as follows:

    (ext:search-list "code:")

returns the list ("/usr/lisp/code/").
]#


;;;; HOST structures

;;; The host structure holds the functions that both parse the pathname
;;; information into sturcture slot entries, and after translation the
;;; inverse (unparse) functions.
;;;
(defstruct (host
	    (:print-function %print-host))
  (parse (required-argument) :type function)
  (unparse (required-argument) :type function)
  (unparse-host (required-argument) :type function)
  (unparse-directory (required-argument) :type function)
  (unparse-file (required-argument) :type function)
  (unparse-enough (required-argument) :type function)
  (customary-case (required-argument) :type (member :upper :lower)))

;;; %PRINT-HOST -- Internal
;;;
(defun %print-host (host stream depth)
  (declare (ignore depth))
  (print-unreadable-object (host stream :type t :identity t)))

(defstruct (logical-host
	    (:include host
		      (:parse #'parse-logical-namestring)
		      (:unparse #'unparse-logical-namestring)
		      (:unparse-host
		       #'(lambda (x) (logical-host-name (%pathname-host x))))
		      (:unparse-directory #'unparse-logical-directory)
		      (:unparse-file #'unparse-unix-file)
		      (:unparse-enough #'unparse-enough-namestring)
		      (:customary-case :upper)))
  (name "" :type simple-base-string)
  (translations nil :type list)
  (canon-transls nil :type list))

;;; The various magic tokens that are allowed to appear in pretty much all
;;; pathname components.
;;;
(deftype component-tokens ()
  '(member nil :unspecific :wild))

;;;; Pathname structures

(defstruct (pathname
	    (:conc-name %pathname-)
	    (:print-function %print-pathname)
	    (:constructor
	     %make-pathname (host device directory name type version))
	    (:predicate pathnamep)
	    (:make-load-form-fun :just-dump-it-normally))
  ;; Slot holds the host, at present either a Unix or logical host.
  (host nil :type (or host null))
  ;; Device is the name of a logical or physical device holding files.
  (device nil :type (or simple-string component-tokens))
  ;; A list of strings that are the component subdirectory components.
  (directory nil :type list)
  ;; The filename.
  (name nil :type (or simple-string pattern component-tokens))
  ;; The type extension of the file.
  (type nil :type (or simple-string pattern component-tokens))
  ;; The version number of the file, a positive integer, but not supported
  ;; on standard Unix filesystems.
  (version nil :type (or integer component-tokens (member :newest))))

;;; %PRINT-PATHNAME -- Internal
;;;
;;; The printed representation of the pathname structure.
;;;
(defun %print-pathname (pathname stream depth)
  (declare (ignore depth))
  (let ((namestring (handler-case (namestring pathname)
		      (error nil))))
    (cond (namestring
	   (if (or *print-escape* *print-readably*)
	       (format stream "#p~S" namestring)
	       (format stream "~A" namestring)))
	  (*print-readably*
	   (error "~S Cannot be printed readably." pathname))
	  (t
	   (funcall (formatter "#<Unprintable pathname, Host=~S, Device=~S, ~
				Directory=~S, Name=~S, Type=~S, Version=~S>")
		    stream
		    (%pathname-host pathname)
		    (%pathname-device pathname)
		    (%pathname-directory pathname)
		    (%pathname-name pathname)
		    (%pathname-type pathname)
		    (%pathname-version pathname))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Logical pathnames have the following format:
;;;
;;; logical-namestring ::=
;;;         [host ":"] [";"] {directory ";"}* [name] ["." type ["." version]]
;;;
;;; host ::= word
;;; directory ::= word | wildcard-word | **
;;; name ::= word | wildcard-word
;;; type ::= word | wildcard-word
;;; version ::= pos-int | newest | NEWEST | *
;;; word ::= {uppercase-letter | digit | -}+
;;; wildcard-word ::= [word] '* {word '*}* [word]
;;; pos-int ::= integer > 0
;;;
;;; Physical pathnames include all these slots and a device slot.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Logical pathnames are a subclass of pathname, their class relations are
;; mimiced using structures for efficiency.

(defstruct (logical-pathname
	    (:conc-name %logical-pathname-)
	    (:print-function %print-logical-pathname)
	    (:include pathname)
	    (:constructor %make-logical-pathname
			  (host device directory name type version))
	    (:make-load-form-fun :just-dump-it-normally)))

(declaim (freeze-type logical-pathname logical-host))

;;; %PRINT-LOGICAL-PATHNAME -- Internal
;;;
;;;   The printed representation of the logical-pathname structure.
;;; The potential conflict with search-lists requires isolating the printed
;;; representation to use the i/o macro #.(logical-pathname <path-designator>).
;;;
(defun %print-logical-pathname (pathname stream depth)
  (declare (ignore depth))
  (let ((namestring (handler-case (namestring pathname)
		      (error nil))))
    (cond (namestring
	   (if (or *print-escape* *print-readably*)
	       (format stream "#.(logical-pathname ~S)" namestring)
	       (format stream "~A" namestring)))
	  (*print-readably*
	   (error "~S Cannot be printed readably." pathname))
	  (t
	   (funcall (formatter "#<Unprintable pathname, Host=~S,  ~
				Directory=~S, File=~S, Name=~S, Version=~S>")
		    stream
		    (%pathname-host pathname)
		    (%pathname-directory pathname)
		    (%pathname-name pathname)
		    (%pathname-type pathname)
		    (%pathname-version pathname))))))

;;; %MAKE-PATHNAME-OBJECT -- internal
;;;
;;; A pathname is logical if the host component is a logical-host.
;;; This constructor is used to make an instance of the correct type
;;; from parsed arguments.

(defun %make-pathname-object (host device directory name type version)
  (if (typep host 'logical-host)
      (%make-logical-pathname host :unspecific directory name type version)
      (%make-pathname         host device      directory name type version)))

;;; *LOGICAL-HOSTS* --internal.
;;;
;;; Hash table searching maps a logical-pathname's host to their physical
;;; pathname translation.

(defvar *logical-hosts* (make-hash-table :test #'equal))

;;; PATH-DESIGNATOR -- internal type
;;;
(deftype path-designator ()
  "A path specification, either a string, stream or pathname."
  '(or string stream pathname))


;;;; Patterns
;;;
;;; A pattern is a list of entries and wildcards used for pattern matches
;;; of translations.

(defstruct (pattern
	    (:print-function %print-pattern)
	    (:make-load-form-fun :just-dump-it-normally)
	    (:constructor make-pattern (pieces)))
  (pieces nil :type list))

;;; %PRINT-PATTERN -- Internal
;;;
(defun %print-pattern (pattern stream depth)
  (declare (ignore depth))
  (print-unreadable-object (pattern stream :type t)
    (if *print-pretty*
	(let ((*print-escape* t))
	  (pprint-fill stream (pattern-pieces pattern) nil))
	(prin1 (pattern-pieces pattern) stream))))

;;; PATTERN= -- Internal
;;;
(defun pattern= (pattern1 pattern2)
  (declare (type pattern pattern1 pattern2))
  (let ((pieces1 (pattern-pieces pattern1))
	(pieces2 (pattern-pieces pattern2)))
    (and (= (length pieces1) (length pieces2))
	 (every #'(lambda (piece1 piece2)
		    (typecase piece1
		      (simple-string
		       (and (simple-string-p piece2)
			    (string= piece1 piece2)))
		      (cons
		       (and (consp piece2)
			    (eq (car piece1) (car piece2))
			    (string= (cdr piece1) (cdr piece2))))
		      (t
		       (eq piece1 piece2))))
		pieces1
		pieces2))))

;;; PATTERN-MATCHES -- Internal
;;;
;;; If the string matches the pattern returns the multiple values T and a
;;; list of the matched strings.
;;;
(defun pattern-matches (pattern string)
  (declare (type pattern pattern)
	   (type simple-string string))
  (let ((len (length string)))
    (labels ((maybe-prepend (subs cur-sub chars)
	       (if cur-sub
		   (let* ((len (length chars))
			  (new (make-string len))
			  (index len))
		     (dolist (char chars)
		       (setf (schar new (decf index)) char))
		     (cons new subs))
		   subs))
	     (matches (pieces start subs cur-sub chars)
	       (if (null pieces)
		   (if (= start len)
		       (values t (maybe-prepend subs cur-sub chars))
		       (values nil nil))
		   (let ((piece (car pieces)))
		     (etypecase piece
		       (simple-string
			(let ((end (+ start (length piece))))
			  (and (<= end len)
			       (string= piece string
					:start2 start :end2 end)
			       (matches (cdr pieces) end
					(maybe-prepend subs cur-sub chars)
					nil nil))))
		       (list
			(ecase (car piece)
			  (:character-set
			   (and (< start len)
				(let ((char (schar string start)))
				  (if (find char (cdr piece) :test #'char=)
				      (matches (cdr pieces) (1+ start) subs t
					       (cons char chars))))))))
		       ((member :single-char-wild)
			(and (< start len)
			     (matches (cdr pieces) (1+ start) subs t
				      (cons (schar string start) chars))))
		       ((member :multi-char-wild)
			(multiple-value-bind
			    (won new-subs)
			    (matches (cdr pieces) start subs t chars)
			  (if won
			      (values t new-subs)
			      (and (< start len)
				   (matches pieces (1+ start) subs t
					    (cons (schar string start)
						  chars)))))))))))
      (multiple-value-bind
	  (won subs)
	  (matches (pattern-pieces pattern) 0 nil nil nil)
	(values won (reverse subs))))))

;;; DIRECTORY-COMPONENTS-MATCH  --  Internal
;;;
;;; Pathname-match-p for directory components. If thing is empty then it
;;; matches :wild, (:absolute :wild-inferiors), or (:relative
;;; :wild-inferiors).
;;;
(defun directory-components-match (thing wild)
  (or (eq thing wild)
      (eq wild :wild)
      (and (consp wild)
	   (let ((wild1 (first wild)))
	     (cond ((and (null thing) (member wild1 '(:absolute :relative)))
		    (equal (rest wild) '(:wild-inferiors)))
		   ((eq wild1 :wild-inferiors)
		    (let ((wild-subdirs (rest wild)))
		      (or (null wild-subdirs)
			  (loop
			   (when (directory-components-match thing
							     wild-subdirs)
			     (return t))
			   (pop thing)
			   (unless thing (return nil))))))
		   ((consp thing)
		    (and (components-match (first thing) wild1)
			 (directory-components-match (rest thing)
						     (rest wild)))))))))

;;; COMPONENTS-MATCH -- Internal
;;;
;;; Return true if pathname component Thing is matched by Wild.  Not
;;; commutative.
;;;
(defun components-match (thing wild)
  (declare (type (or pattern symbol simple-string integer) thing wild))
  (or (eq thing wild)
      (eq wild :wild)
      (typecase thing
	(simple-base-string
	 ;; String is matched by itself, a matching pattern or :WILD.
	 (typecase wild
	   (pattern
	    (values (pattern-matches wild thing)))
	   (simple-base-string
	    (string= thing wild))))
	(pattern
	 ;; A pattern is only matched by an identical pattern.
	 (and (pattern-p wild) (pattern= thing wild)))
	(integer
	 ;; an integer (version number) is matched by :WILD or the same
	 ;; integer.  This branch will actually always be NIL as long as
	 ;; the version is a fixnum.
	 (eql thing wild)))))

;;; COMPARE-COMPONENT  -- Internal
;;;
;;; A predicate for comparing two pathname slot component sub-entries.
;;;
(defun compare-component (this that)
  (or (eql this that)
      (typecase this
	(simple-string
	 (and (simple-string-p that)
	      (string= this that)))
	(pattern
	 (and (pattern-p that)
	      (pattern= this that)))
	(cons
	 (and (consp that)
	      (compare-component (car this) (car that))
	      (compare-component (cdr this) (cdr that)))))))


;;;; Pathname functions.

(defun relativep (pathname)
  "Return #t if $pathname is a relative pathname."
  (or pathname (error "$pathname ()"))
  (let ((dir (pathname-directory (pathname pathname))))
    (if dir
	(eq (car dir) :relative)
	t)))

(defun absolutep (pathname)
  "Return #t if $pathname is an absolute pathname."
  (or pathname (error "$pathname ()"))
  (let ((dir (pathname-directory (pathname pathname))))
    (if dir (eq (car dir) :absolute))))

(defun circular-p (pathname)
  "Return true if the end of $pathname makes it circular.

   A pathname if circular if a component of the pathname is a link to an
   earlier component of the pathname, for example

      /a/b/c/

   when c is a symlink to /a/."
  (and (symlinkp pathname)
       (let ((truename (truename pathname)))
	 (iterate check ((dirs (pathname-directory
				(merge-pathnames
				 (namify pathname)
				 (current-directory)))))
	   (if (equal (truename (make-pathname
				 :directory dirs
				 :defaults truename))
		      truename)
	       (return t))
	   (if (cdr dirs)
	       (check (butlast dirs)))))))

(defun remote-pathname-p (pathname)
  "Return true if $pathname names a remote file, that is, if $pathname
   contains a search list outside the set of defined search lists."
  (let ((name (namestring (if (if (pathname-directory pathname)
				  (eq (car (pathname-directory pathname))
				      :relative)
				  t)
			      (current-directory)
			      pathname))))
    (and (extract-search-list name ())
	 (fi (search-list-defined-p name)))))

(defun remote-pathname-host (pathname)
  "Return the host name of remote $pathname."
  (search-list-name
   (or (extract-search-list
	(if (if (pathname-directory pathname)
		(eq (car (pathname-directory pathname)) :relative)
		t)
	    (current-directory)
	    pathname)
	())
       (return-from remote-pathname-host ()))))

(defun remote-pathname-local (pathname)
  "Return the local part of remote $pathname."
  (let* ((namestring (namestring (merge-pathnames pathname (current-directory))))
	 (search-list (extract-search-list namestring ())))
    (if search-list
	(subseq namestring (1+ (length (search-list-name search-list))))
	namestring)))

;;; Implementation determined defaults to pathname slots.  Set in
;;; `filesys-init'.

(defvar *default-pathname-defaults*)

;;; PATHNAME= -- Internal
;;;
(defun pathname= (pathname1 pathname2)
  (declare (type pathname pathname1)
	   (type pathname pathname2))
  (and (eq (%pathname-host pathname1)
	   (%pathname-host pathname2))
       (compare-component (%pathname-device pathname1)
			  (%pathname-device pathname2))
       (compare-component (%pathname-directory pathname1)
			  (%pathname-directory pathname2))
       (compare-component (%pathname-name pathname1)
			  (%pathname-name pathname2))
       (compare-component (%pathname-type pathname1)
			  (%pathname-type pathname2))
       (compare-component (%pathname-version pathname1)
			  (%pathname-version pathname2))))

;;; WITH-PATHNAME -- Internal
;;;
;;; Converts the expr, a pathname designator (a pathname, or string, or
;;; stream), into a pathname.
;;;
(defmacro with-pathname ((var expr) &body body)
  `(let ((,var (let ((,var ,expr))
		 (etypecase ,var
		   (pathname ,var)
		   (string (parse-namestring ,var))
		   (stream (file-name ,var))))))
     ,@body))

;;; WITH-HOST -- Internal
;;;
;;; Converts the var, a host or string name for a host, into a logical-host
;;; structure or nil if not defined.
;;;
;;; FIX pw notes 1/12/97 this potentially useful macro is not used anywhere
;;; and 'find-host' is not defined. 'find-logical-host' seems to be needed.
;;;
(defmacro with-host ((var expr) &body body)
  `(let ((,var (let ((,var ,expr))
		 (typecase ,var
		   (logical-host ,var)
		   (string (find-logical-host ,var nil))
		   (t nil)))))
     ,@body))

;;; PATHNAME -- Interface
;;;
(defun pathname (thing)
  "Convert thing (a pathname, string or stream) into a pathname."
  (declare (type path-designator thing))
  (with-pathname (pathname thing)
    pathname))

;;; MAYBE-DIDDLE-CASE  -- Internal
;;;
;;; Change the case of thing if diddle-p T.
;;;
(defun maybe-diddle-case (thing diddle-p)
  (if (and diddle-p (not (or (symbolp thing) (integerp thing))))
      (labels ((check-for (pred in)
		 (typecase in
		   (pattern
		    (dolist (piece (pattern-pieces in))
		      (when (typecase piece
			      (simple-string
			       (check-for pred piece))
			      (cons
			       (case (car in)
				 (:character-set
				  (check-for pred (cdr in))))))
			(return t))))
		   (list
		    (dolist (x in)
		      (when (check-for pred x)
			(return t))))
		   (simple-base-string
		    (dotimes (i (length in))
		      (when (funcall pred (schar in i))
			(return t))))
		   (t nil)))
	       (diddle-with (fun thing)
		 (typecase thing
		   (pattern
		    (make-pattern
		     (mapcar #'(lambda (piece)
				 (typecase piece
				   (simple-base-string
				    (funcall fun piece))
				   (cons
				    (case (car piece)
				      (:character-set
				       (cons :character-set
					     (funcall fun (cdr piece))))
				      (t
				       piece)))
				   (t
				    piece)))
			     (pattern-pieces thing))))
		   (list
		    (mapcar fun thing))
		   (simple-base-string
		    (funcall fun thing))
		   (t
		    thing))))
	(let ((any-uppers (check-for #'upper-case-p thing))
	      (any-lowers (check-for #'lower-case-p thing)))
	  (cond ((and any-uppers any-lowers)
		 ;; Mixed case, stays the same.
		 thing)
		(any-uppers
		 ;; All uppercase, becomes all lower case.
		 (diddle-with #'(lambda (x) (if (stringp x)
						(string-downcase x)
						x)) thing))
		(any-lowers
		 ;; All lowercase, becomes all upper case.
		 (diddle-with #'(lambda (x) (if (stringp x)
						(string-upcase x)
						x)) thing))
		(t
		 ;; No letters?  I guess just leave it.
		 thing))))
      thing))

;;; MERGE-DIRECTORIES -- Internal
;;;
(defun merge-directories (dir1 dir2 diddle-case)
  (if (or (eq (car dir1) :absolute)
	  (null dir2))
      dir1
      (let ((results nil))
	(flet ((add (dir)
		 (if (and (eq dir :back)
			  results
			  (not (eq (car results) :back)))
		     (pop results)
		     (push dir results))))
	  (dolist (dir (maybe-diddle-case dir2 diddle-case))
	    (add dir))
	  (dolist (dir (cdr dir1))
	    (add dir)))
	(reverse results))))

;;; MERGE-PATHNAMES -- Interface
;;;
(defun merge-pathnames (pathname
			&optional
			(defaults *default-pathname-defaults*)
			(default-version :newest))
  "Construct a filled in pathname by completing the unspecified components
   from the defaults."
  (declare (type path-designator pathname)
	   (type path-designator defaults)
	   (values pathname))
  ;; FIX think must special case hidden files
  ;;       so (rename ".db.TEM" ".db") produces ".db"
  (with-pathname (defaults defaults)
    (let ((pathname (let ((*default-pathname-defaults* defaults))
		      (pathname pathname))))
      (let* ((default-host (%pathname-host defaults))
	     (pathname-host (%pathname-host pathname))
	     (diddle-case
	      (and default-host pathname-host
		   (not (eq (host-customary-case default-host)
			    (host-customary-case pathname-host))))))
	(%make-pathname-object
	 (or pathname-host default-host)
	 (or (%pathname-device pathname)
	     (maybe-diddle-case (%pathname-device defaults)
				diddle-case))
	 (merge-directories (%pathname-directory pathname)
			    (%pathname-directory defaults)
			    diddle-case)
	 (or (%pathname-name pathname)
	     (maybe-diddle-case (%pathname-name defaults)
				diddle-case))
	 (or (%pathname-type pathname)
	     (maybe-diddle-case (%pathname-type defaults)
				diddle-case))
	 (or (%pathname-version pathname)
	     default-version))))))

;;; IMPORT-DIRECTORY -- Internal
;;;
(defun import-directory (directory diddle-case)
  (etypecase directory
    (null nil)
    ((member :wild) '(:absolute :wild-inferiors))
    ((member :unspecific) '(:relative))
    (list
     (collect ((results))
       (ecase (pop directory)
	 (:absolute
	  (results :absolute)
	  (when (search-list-p (car directory))
	    (results (pop directory))))
	 (:relative
	  (results :relative)))
       (dolist (piece directory)
	 (cond ((member piece '(:wild :wild-inferiors :up :back))
		(results piece))
	       ((or (simple-string-p piece) (pattern-p piece))
		(results (maybe-diddle-case piece diddle-case)))
	       ((stringp piece)
		(results (maybe-diddle-case (coerce piece 'simple-string)
					    diddle-case)))
	       (t
		(error "~S is not allowed as a directory component." piece))))
       (results)))
    (simple-string
     `(:absolute
       ,(maybe-diddle-case directory diddle-case)))
    (string
     `(:absolute
       ,(maybe-diddle-case (coerce directory 'simple-string)
			   diddle-case)))))

;;; MAKE-PATHNAME -- Interface
;;;
(defun make-pathname (&key host
			   (device nil devp)
			   (directory nil dirp)
			   (name nil namep)
			   (type nil typep)
			   (version nil versionp)
			   defaults
			   (case :local))
  "Makes a new pathname from the component arguments.  Host can be a
   host-structure or string."
  (declare (type (or string host component-tokens) host)
	   (type (or string component-tokens) device)
	   (type (or list string pattern component-tokens) directory)
	   (type (or string pattern component-tokens) name type)
	   (type (or integer component-tokens (member :newest)) version)
	   (type (or path-designator null) defaults)
	   (type (member :common :local) case))
  (let* ((defaults (when defaults
		     (with-pathname (defaults defaults) defaults)))
	 (default-host (if defaults
			   (%pathname-host defaults)
			   (pathname-host *default-pathname-defaults*)))
	 ;; toy@rtp.ericsson.se: CLHS says make-pathname can take a
	 ;; string (as a logical-host) for the host part.  We map that
	 ;; string into the corresponding logical host structure.

	 ;; pw@snoopy.mv.com:
	 ;; HyperSpec says for the arg to MAKE-PATHNAME;
	 ;; "host---a valid physical pathname host. ..."
	 ;; where it probably means -- a valid pathname host.
	 ;; "valid pathname host n. a valid physical pathname host or
	 ;; a valid logical pathname host."
	 ;; and defines
	 ;; "valid physical pathname host n. any of a string,
	 ;; a list of strings, or the symbol :unspecific,
	 ;; that is recognized by the implementation as the name of a host."
	 ;; "valid logical pathname host n. a string that has been defined
	 ;; as the name of a logical host. ..."
	 ;; HS is silent on what happens if the :host arg is NOT one of these.
	 ;; It seems an error message is appropriate.
	 (host (typecase host
		 (host host) 		; A valid host, use it.
		 (string (find-logical-host host t)) ; logical-host or lose.
		 (t default-host)))	; unix-host
	 (diddle-args (and (eq (host-customary-case host) :lower)
			   (eq case :common)))
	 (diddle-defaults
	  (not (eq (host-customary-case host)
		   (host-customary-case default-host))))
	 (dev (if devp device (if defaults (%pathname-device defaults))))
	 (dir (import-directory directory diddle-args))
	 (ver (cond
	       (versionp version)
	       (defaults (%pathname-version defaults))
	       (t nil))))
    (when (and defaults (not dirp))
      (setf dir
	    (merge-directories dir
			       (%pathname-directory defaults)
			       diddle-defaults)))

    (macrolet ((pick (var varp field)
		 `(cond ((or (simple-string-p ,var)
			     (pattern-p ,var))
			 (maybe-diddle-case ,var diddle-args))
			((stringp ,var)
			 (maybe-diddle-case (coerce ,var 'simple-string)
					    diddle-args))
			(,varp
			 (maybe-diddle-case ,var diddle-args))
			(defaults
			 (maybe-diddle-case (,field defaults)
					    diddle-defaults))
			(t
			 nil))))
      (%make-pathname-object host
			     dev ; forced to :unspecific when logical-host
			     dir
			     (pick name namep %pathname-name)
			     (pick type typep %pathname-type)
			     ver))))

;;; PATHNAME-HOST -- Interface
;;;
(defun pathname-host (pathname &key (case :local))
  "Accessor for the pathname's host."
  (declare (type path-designator pathname)
	   (type (member :local :common) case)
	   (values host)
	   (ignore case))
  (with-pathname (pathname pathname)
    (%pathname-host pathname)))

;;; PATHNAME-DEVICE -- Interface
;;;
(defun pathname-device (pathname &key (case :local))
  "Accessor for pathname's device."
  (declare (type path-designator pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-device pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

;;; PATHNAME-DIRECTORY -- Interface
;;;
(defun pathname-directory (pathname &key (case :local))
  "Accessor for $pathname's directory list."
  (declare (type path-designator pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-directory pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

;;; PATHNAME-NAME -- Interface
;;;
(defun pathname-name (pathname &key (case :local))
  "Accessor for the pathname's name."
  (declare (type path-designator pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-name pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

;;; PATHNAME-TYPE
;;;
(defun pathname-type (pathname &key (case :local))
  "Accessor for the pathname's type."
  (declare (type path-designator pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-type pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

;;; PATHNAME-VERSION
;;;
(defun pathname-version (pathname)
  "Accessor for the pathname's version."
  (declare (type path-designator pathname))
  (with-pathname (pathname pathname)
    (%pathname-version pathname)))


;;;; Namestrings

;;; %PRINT-NAMESTRING-PARSE-ERROR -- Internal
;;;
(defun %print-namestring-parse-error (condition stream)
  (format stream "Parse error in namestring: ~?~%  ~A~%  ~V@T^"
	  (namestring-parse-error-complaint condition)
	  (namestring-parse-error-arguments condition)
	  (namestring-parse-error-namestring condition)
	  (namestring-parse-error-offset condition)))

(define-condition namestring-parse-error (parse-error)
  ((complaint :reader namestring-parse-error-complaint :initarg :complaint)
   (arguments :reader namestring-parse-error-arguments :initarg :arguments
	      :initform nil)
   (namestring :reader namestring-parse-error-namestring :initarg :namestring)
   (offset :reader namestring-parse-error-offset :initarg :offset))
  (:report %print-namestring-parse-error))

;;; %PARSE-NAMESTRING -- Internal
;;;
;;; Handle the case where parse-namestring is actually parsing a
;;; namestring.  We pick off the :JUNK-ALLOWED case then find a host to use
;;; for parsing, call the parser, then check if the host matches.
;;;
(defun %parse-namestring (namestr host defaults start end junk-allowed)
  (declare (type string namestr)
	   (type (or host null) host)
	   (type index start)
	   (type (or index null) end))
  (if junk-allowed
      (handler-case
	  (%parse-namestring namestr host defaults start end nil)
	(namestring-parse-error (condition)
	  (values nil (namestring-parse-error-offset condition))))
      (let* ((end (or end (length namestr)))
	     (parse-host (or host
			     (extract-logical-host-prefix namestr start end)
			     (pathname-host defaults))))
	(or parse-host
	    (error "Either $host must be supplied or $defaults must have~
		    a PATHNAME-HOST."))

	(multiple-value-bind
	    (new-host device directory file type version)
	    (funcall (host-parse parse-host) namestr start end)
	  (and host
	       new-host
	       (or (eq new-host host)
		   (error "Host in namestring: ~S~@
			   must match explicit host argument: ~S"
			  host)))
	  (let ((pn-host (or new-host parse-host)))
	    (values (%make-pathname-object
		     pn-host device directory file type version)
		    end))))))

;;; EXTRACT-LOGICAL-HOST-PREFIX -- Internal
;;;
;;; If namestr begins with a colon-terminated, defined, logical host, then
;;; return that host, otherwise return NIL.
;;;
(defun extract-logical-host-prefix (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end)
	   (values (or logical-host null)))
  (let ((colon-pos (position #\: namestr :start start :end end)))
    (if colon-pos
	(values (gethash (nstring-upcase (subseq namestr start colon-pos))
			 *logical-hosts*))
	nil)))

;;; PARSE-NAMESTRING -- Interface
;;;
(defun parse-namestring (pathname
			 &optional host (defaults *default-pathname-defaults*)
			 &key (start 0) end junk-allowed)
  "Convert $pathname, a pathname designator, into a pathname structure, for
   a physical pathname, return the printed representation.  $host may be a
   physical host structure or host namestring."
  (declare (type path-designator pathname)
	   (type (or null host) host)
	   (type pathname defaults)
	   (type index start)
	   (type (or index null) end))
    (etypecase pathname
      (simple-string
       (%parse-namestring pathname host defaults start end junk-allowed))
      (string
       (%parse-namestring (coerce pathname 'simple-string)
			  host defaults start end junk-allowed))
      (pathname
       (let ((host (if host host (%pathname-host defaults))))
	 (or (eq host (%pathname-host pathname))
	     (error "Hosts must match: ~S and ~S."
		    host (%pathname-host pathname))))
       (values pathname start))
      (stream
       (let ((name (file-name pathname)))
	 (or name
	     (error "Can't figure out the file associated with stream:~%  ~S"
		    pathname))
	 (values name nil)))))

;;; NAMESTRING -- Interface
;;;
(defun namestring (pathname)
  "Construct the full (name)string form of the pathname."
  (declare (type path-designator pathname)
	   (values (or null simple-base-string)))
  (with-pathname (pathname pathname)
    (when pathname
      (let ((host (%pathname-host pathname)))
	(or host
	    (error "Cannot determine the namestring for pathnames with no ~
		    host:~%  ~S" pathname))
	(funcall (host-unparse host) pathname)))))

;;; HOST-NAMESTRING -- Interface
;;;
(defun host-namestring (pathname)
  "Returns a string representation of the name of the host in the pathname."
  (declare (type path-designator pathname)
	   (values (or null simple-base-string)))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-host host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

;;; DIRECTORY-NAMESTRING -- Interface
;;;
(defun directory-namestring (pathname)
  "Return a string representation of the directories used in the pathname."
  (declare (type path-designator pathname)
	   (values (or null simple-base-string)))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-directory host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

;;; FILE-NAMESTRING -- Interface
;;;
(defun file-namestring (pathname)
  "Return a string representation of the name used in $pathname."
  (declare (type path-designator pathname)
	   (values (or null simple-base-string)))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-file host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

;;; ENOUGH-NAMESTRING -- Interface
;;;
(defun enough-namestring (pathname
			  &optional (defaults *default-pathname-defaults*))
  "Returns an abbreviated pathname sufficent to identify the pathname relative
   to the defaults."
  (declare (type path-designator pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (with-pathname (defaults defaults)
	    (funcall (host-unparse-enough host) pathname defaults))
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))


;;;; Wild pathnames.

;;; WILD-PATHNAME-P -- Interface
;;;
(defun wild-pathname-p (pathname &optional field-key)
  "Predicate for determining whether pathname contains any wildcards."
  (declare (type path-designator pathname)
	   (type (member nil :host :device :directory :name :type :version)
		 field-key))
  (with-pathname (pathname pathname)
		 (flet ((frob (x)
			  (or (pattern-p x)
			      (member x '(:wild :wild-inferiors)))))
		   (ecase field-key
		     ((nil)
		      (or (wild-pathname-p pathname :host)
			  (wild-pathname-p pathname :device)
			  (wild-pathname-p pathname :directory)
			  (wild-pathname-p pathname :name)
			  (wild-pathname-p pathname :type)
			  (wild-pathname-p pathname :version)))
		     (:host (frob (%pathname-host pathname)))
		     (:device (frob (%pathname-device pathname)))
		     (:directory (some #'frob (%pathname-directory pathname)))
		     (:name (frob (%pathname-name pathname)))
		     (:type (frob (%pathname-type pathname)))
		     (:version (frob (%pathname-version pathname)))))))

;;; COMMON-PREFIX  FIX unique-prefix?
;;;
(defun common-prefix (pathname)
  "Return two values: a pathname representing all of $pathname that comes
   before the first wild directory entry, and the portion of $pathname from
   the first wild directory entry."
  (declare (type path-designator pathname))
  (with-pathname (pathname pathname)
    (collect ((prefix-dir))
      (let (suffix)
	(loop for node = (pathname-directory pathname) then (cdr node)
	  while node
	  do
	  (when (member (car node) '(:wild :wild-inferiors))
	    (setq suffix node)
	    (return))
	  (prefix-dir (car node)))
	(flet ((or-wild (x) (fi (or (eq x :wild)
				    (pattern-p x))
				x)))
	  (let* ((name (fi suffix (or-wild (pathname-name pathname))))
		 (type (if name (or-wild (pathname-type pathname)))))
	    (values (make-pathname :host (pathname-host pathname)
				   :device (pathname-device pathname) ; FIX can be wild?
				   :directory (prefix-dir)
				   :name name
				   :type type
				   :version (fi type (or-wild (pathname-version pathname))))
		    (make-pathname :directory (cons :relative suffix)
				   :name (pathname-name pathname)
				   :type (pathname-type pathname)
				   :version (pathname-version pathname)))))))))

;;; PATHNAME-MATCH-P -- Interface
;;;
(defun pathname-match-p (in-pathname in-wildname)
  "Return true if $in-pathname matches template $in-wildname."
  (declare (type path-designator in-pathname))
  (with-pathname (pathname in-pathname)
    (with-pathname (wildname in-wildname)
      (macrolet ((frob (field &optional (op 'components-match ))
		   `(or (null (,field wildname))
			(,op (,field pathname) (,field wildname)))))
	(and (or (null (%pathname-host wildname))
		 (eq (%pathname-host wildname) (%pathname-host pathname)))
	     (frob %pathname-device)
	     (frob %pathname-directory directory-components-match)
	     (frob %pathname-name)
	     (frob %pathname-type)
	     (frob %pathname-version))))))

;;; SUBSTITUTE-INTO -- Internal
;;;
;;; Place the substitutions into the pattern and return the string or
;;; pattern that results.  If DIDDLE-CASE is true, we diddle the result
;;; case as well, in case we are translating between hosts with difference
;;; conventional case.  The second value is the tail of subs with all of
;;; the values that we used up stripped off.  Note that PATTERN-MATCHES
;;; matches all consecutive wildcards as a single string, so we ignore
;;; subsequent contiguous wildcards.
;;;
(defun substitute-into (pattern subs diddle-case)
  (declare (type pattern pattern)
	   (type list subs)
	   (values (or simple-base-string pattern) list))
  (let ((in-wildcard nil)
	(pieces nil)
	(strings nil))
    (dolist (piece (pattern-pieces pattern))
      (cond ((simple-string-p piece)
	     (push piece strings)
	     (setf in-wildcard nil))
	    (in-wildcard)
	    (t
	     (setf in-wildcard t)
	     (unless subs
	       (error "Not enough wildcards in FROM pattern to match ~
		       TO pattern:~%  ~S"
		      pattern))
	     (let ((sub (pop subs)))
	       (typecase sub
		 (pattern
		  (when strings
		    (push (apply #'concatenate 'simple-string
				 (nreverse strings))
			  pieces))
		  (dolist (piece (pattern-pieces sub))
		    (push piece pieces)))
		 (simple-string
		  (push sub strings))
		 (t
		  (error "Can't substitute this into the middle of a word:~
			  ~%  ~S"
			 sub)))))))

    (when strings
      (push (apply #'concatenate 'simple-string (nreverse strings))
	    pieces))
    (values
     (maybe-diddle-case
      (if (and pieces (simple-string-p (car pieces)) (null (cdr pieces)))
	  (car pieces)
	  (make-pattern (nreverse pieces)))
      diddle-case)
     subs)))

;;; DIDNT-MATCH-ERROR  --  Internal
;;;
;;;    Called when we can't see how source and from matched.
;;;
(defun didnt-match-error (source from)
  (error "Pathname components from Source and From args to TRANSLATE-PATHNAME~@
	  did not match:~%  ~S ~S"
	 source from))

;;; TRANSLATE-COMPONENT -- Internal
;;;
;;;   Do TRANSLATE-COMPONENT for all components except host and directory.
;;;
(defun translate-component (source from to diddle-case)
  (typecase to
    (pattern
     (typecase from
       (pattern
	(typecase source
	  (pattern
	   (if (pattern= from source)
	       source
	       (didnt-match-error source from)))
	  (simple-string
	   (multiple-value-bind
	       (won subs)
	       (pattern-matches from source)
	     (if won
		 (values (substitute-into to subs diddle-case))
		 (didnt-match-error source from))))
	  (t
	   (maybe-diddle-case source diddle-case))))
       ((member :wild)
	(values (substitute-into to (list source) diddle-case)))
       (t
	(if (components-match source from)
	    (maybe-diddle-case source diddle-case)
	    (didnt-match-error source from)))))
    ((member nil :wild)
     (maybe-diddle-case source diddle-case))
    (t
     (if (components-match source from)
	 to
	 (didnt-match-error source from)))))

;;; COMPUTE-DIRECTORY-SUBSTITUTIONS  --  Internal
;;;
;;;    Return a list of all the things that we want to substitute into the TO
;;; pattern (the things matched by from on source.)  When From contains
;;; :WILD-INFERIORS, the result contains a sublist of the matched source
;;; subdirectories.
;;;
(defun compute-directory-substitutions (orig-source orig-from)
  (let ((source orig-source)
	(from orig-from))
    (collect ((subs))
      (loop
	(unless source
	  (unless (every #'(lambda (x) (eq x :wild-inferiors)) from)
	    (didnt-match-error orig-source orig-from))
	  (subs ())
	  (return))
	(unless from (didnt-match-error orig-source orig-from))
	(let ((from-part (pop from))
	      (source-part (pop source)))
	  (typecase from-part
	    (pattern
	     (typecase source-part
	       (pattern
		(if (pattern= from-part source-part)
		    (subs source-part)
		    (didnt-match-error orig-source orig-from)))
	       (simple-string
		(multiple-value-bind
		    (won new-subs)
		    (pattern-matches from-part source-part)
		  (if won
		      (dolist (sub new-subs)
			(subs sub))
		      (didnt-match-error orig-source orig-from))))
	       (t
		(didnt-match-error orig-source orig-from))))
	    ((member :wild)
	     (subs source-part))
	    ((member :wild-inferiors)
	     (let ((remaining-source (cons source-part source)))
	       (collect ((res))
		 (loop
		   (when (directory-components-match remaining-source from)
		     (return))
		   (unless remaining-source
		     (didnt-match-error orig-source orig-from))
		   (res (pop remaining-source)))
		 (subs (res))
		 (setq source remaining-source))))
	    (simple-string
	     (unless (and (simple-string-p source-part)
			  (string= from-part source-part))
	       (didnt-match-error orig-source orig-from)))
	    (t
	     (didnt-match-error orig-source orig-from)))))
      (subs))))

;;; TRANSLATE-DIRECTORIES -- Internal
;;;
;;;    Called by TRANSLATE-PATHNAME on the directory components of its argument
;;; pathanames to produce the result directory component.  If any leaves the
;;; directory NIL, we return the source directory.  The :RELATIVE or :ABSOLUTE
;;; is taken from the source directory, except if TO is :ABSOLUTE, in which
;;; case the result will be :ABSOLUTE.
;;;
(defun translate-directories (source from to diddle-case)
  (if (not (and source to from))
      (let ((source (mapcar #'(lambda (x) (maybe-diddle-case x diddle-case))
			    source)))
	(if (null to)
	    source
	    (collect ((res))
	      (res (cond ((null source) (first to))
			 ((eq (first to) :absolute) :absolute)
			 (t (first source))))
	      (let ((match (rest source)))
		(dolist (to-part (rest to))
		  (cond ((eq to-part :wild)
			 (when match
			   (res (first match))
			   (setf match nil)))
			((eq to-part :wild-inferiors)
			 (when match
			   (dolist (src-part match)
			     (res src-part))
			   (setf match nil)))
			(t
			 (res to-part)))))
	      (res))))
      (collect ((res))
	(res (if (eq (first to) :absolute)
		 :absolute
		 (first source)))
	(let ((subs-left (compute-directory-substitutions (rest source)
							  (rest from))))
	  (dolist (to-part (rest to))
	    (typecase to-part
	      ((member :wild)
	       (assert subs-left)
	       (let ((match (pop subs-left)))
		 (when (listp match)
		   (error ":WILD-INFERIORS not paired in from and to ~
			   patterns:~%  ~S ~S" from to))
		 (res (maybe-diddle-case match diddle-case))))
	      ((member :wild-inferiors)
	       (assert subs-left)
	       (let ((match (pop subs-left)))
		 (unless (listp match)
		   (error ":WILD-INFERIORS not paired in from and to ~
			   patterns:~%  ~S ~S" from to))
		 (dolist (x match)
		   (res (maybe-diddle-case x diddle-case)))))
	      (pattern
	       (multiple-value-bind
		   (new new-subs-left)
		   (substitute-into to-part subs-left diddle-case)
		 (setf subs-left new-subs-left)
		 (res new)))
	      (t (res to-part)))))
	(res))))

;;; TRANSLATE-PATHNAME -- Interface
;;;
(defun translate-pathname (source from-wildname to-wildname &key)
  "Use the source pathname to translate the from-wildname's wild and
   unspecified elements into a completed to-pathname based on the to-wildname."
  (declare (type path-designator source from-wildname to-wildname))
  (with-pathname (source source)
    (with-pathname (from from-wildname)
      (with-pathname (to to-wildname)
	  (let* ((source-host (%pathname-host source))
		 (to-host (%pathname-host to))
		 (diddle-case
		  (and source-host to-host
		       (not (eq (host-customary-case source-host)
				(host-customary-case to-host))))))
	    (macrolet ((frob (field &optional (op 'translate-component))
			 `(let ((result (,op (,field source)
					     (,field from)
					     (,field to)
					     diddle-case)))
			    (if (eq result :error)
				(error "~S doesn't match ~S" source from)
				result))))
	      (%make-pathname-object
	       (or to-host source-host)
	       (frob %pathname-device)
	       (frob %pathname-directory translate-directories)
	       (frob %pathname-name)
	       (frob %pathname-type)
	       (frob %pathname-version))))))))


;;;; Search lists.

;;; The SEARCH-LIST structure.
;;;
(defstruct (search-list
	    (:print-function %print-search-list)
	    (:make-load-form-fun
	     (lambda (search-list)
	       (values `(intern-search-list ',(search-list-name search-list))
		       nil))))
  ;;
  ;; The name of this search-list.  Always stored in lowercase.
  (name (required-argument) :type simple-string)
  ;;
  ;; T if this search-list has been defined.  Otherwise NIL.
  (defined nil :type (member t nil))
  ;;
  ;; The list of expansions for this search-list.  Each expansion is the list
  ;; of directory components to use in place of this search-list.
  (expansions nil :type list))

(defun %print-search-list (sl stream depth)
  (declare (ignore depth))
  (print-unreadable-object (sl stream :type t)
    (write-string (search-list-name sl) stream)))

;;; *SEARCH-LISTS* -- internal.
;;;
;;; Hash table mapping search-list names to search-list structures.
;;;
(defvar *search-lists* (make-hash-table :test #'equal))

;;; INTERN-SEARCH-LIST -- internal interface.
;;;
;;; When search-lists are encountered in namestrings, they are converted to
;;; search-list structures right then, instead of waiting until the search
;;; list used.  This allows us to verify ahead of time that there are no
;;; circularities and makes expansion much quicker.
;;;
(defun intern-search-list (name)
  (let ((name (string-downcase name)))
    (or (gethash name *search-lists*)
	(let ((new (make-search-list :name name)))
	  (setf (gethash name *search-lists*) new)
	  new))))

;;; CLEAR-SEARCH-LIST -- public.
;;;
;;; Clear the definition.  Note: we can't remove it from the hash-table
;;; because there may be pathnames still refering to it.  So we just clear
;;; out the expansions and set defined to NIL.
;;;
(defun clear-search-list (name)
  "Clear the current definition for the search-list $name.  Return t if
   such a definition existed, and () otherwise."
  (let* ((name (string-downcase name))
	 (search-list (gethash name *search-lists*)))
    (when (and search-list (search-list-defined search-list))
      (setf (search-list-defined search-list) nil)
      (setf (search-list-expansions search-list) nil)
      t)))

;;; CLEAR-ALL-SEARCH-LISTS -- sorta public.
;;;
;;; Again, we can't actually remove the entries from the hash-table, so we
;;; just mark them as being undefined.
;;;
(defun clear-all-search-lists ()
  "Clear the definition for all search-lists.  Only use this if you know
   what you are doing."
  (maphash #'(lambda (name search-list)
	       (declare (ignore name))
	       (setf (search-list-defined search-list) nil)
	       (setf (search-list-expansions search-list) nil))
	   *search-lists*)
  nil)

;;; EXTRACT-SEARCH-LIST -- internal.
;;;
;;; Extract the search-list from PATHNAME and return it.  If PATHNAME
;;; doesn't start with a search-list, then either error (if FLAME-IF-NONE
;;; is true) or return NIL (if FLAME-IF-NONE is false).
;;;
(defun extract-search-list (pathname flame-if-none)
  (with-pathname (pathname pathname)
    (let* ((directory (%pathname-directory pathname))
	   (search-list (cadr directory)))
      (cond ((search-list-p search-list)
	     search-list)
	    (flame-if-none
	     (error "~S doesn't start with a search-list." pathname))
	    (t
	     nil)))))

;;; SEARCH-LIST -- public.
;;;
;;; We have to convert the internal form of the search-list back into a
;;; bunch of pathnames.
;;;
(defun search-list (pathname)
  "Return the list of directories associated with the search list
   $pathname.

   If $pathname is something other than a defined search list or starts
   with something other than a search list, then signal an error.

   When set with `setf', the list of directories is changed to the new
   value.  If the new value is just a namestring or pathname, then it is
   interpreted as a one-element list.  Search list names are case-folded."
  (with-pathname (pathname pathname)
    (let ((search-list (extract-search-list pathname t))
	  (host (pathname-host pathname)))
      (if (search-list-defined search-list)
	  (mapcar #'(lambda (directory)
		      (make-pathname :host host
				     :directory (cons :absolute directory)))
		  (search-list-expansions search-list))
	  (error "Search list ~S has not been defined yet." pathname)))))

;;; SEARCH-LIST-DEFINED-P -- public.
;;;
(defun search-list-defined-p (pathname &optional (must-exist t))
  "Return t if the search-list starting $pathname is currently defined,
   else ().

   $pathname should start with a search-list, otherwise signal an error."
  (declare (ignore must-exist)) ; FIX
  (with-pathname (pathname pathname)
    (search-list-defined (extract-search-list pathname t))))

;;; %SET-SEARCH-LIST -- public setf method
;;;
;;; Set the expansion for the search-list in PATHNAME.  If this would result
;;; in any circularities, we flame out.  If anything goes wrong, we leave the
;;; old defintion intact.
;;;
(defun %set-search-list (pathname values)
  (let ((search-list (extract-search-list pathname t)))
    (labels
	((check (target-list path)
	   (if (eq search-list target-list)
	       (error "That would result in a circularity:~%  ~
		       ~A~{ -> ~A~} -> ~A"
		      (search-list-name search-list)
		      (reverse path)
		      (search-list-name target-list)))
	   (when (search-list-p target-list)
	     (push (search-list-name target-list) path)
	     (dolist (expansion (search-list-expansions target-list))
	       (check (car expansion) path))))
	 (convert (pathname)
	   (with-pathname (pathname pathname)
	     (if (or (pathname-name pathname)
		     (pathname-type pathname)
		     (pathname-version pathname))
		 (error "Search-lists cannot expand into pathnames that have ~
			 a name, type, or ~%version specified:~%  ~S"
			pathname))
	     (let ((directory (pathname-directory pathname)))
	       (let ((expansion
		      (if directory
			  (ecase (car directory)
			    (:absolute (cdr directory))
			    (:relative ;(intern-search-list "default")
			     (list "")))
			  ;(list (intern-search-list "default"))
			  (list ""))))
		 (check (car expansion) nil)
		 expansion)))))
      (setf (search-list-expansions search-list)
	    (if (listp values)
		(mapcar #'convert values)
		(list (convert values)))))
    (setf (search-list-defined search-list) t))
  values)

;;; ENUMERATE-SEARCH-LIST -- public.
;;;
(defmacro enumerate-search-list ((var pathname &optional result) &body body)
  "enumerate-search-list (var pathname [result]) body

   An interface to search list resolution.  Execute $body with $var bound
   to each successive possible expansion for $pathname and then return
   $result.

   If $pathname contains something other than a search-list, then execute
   $body exactly once.

   Wrap everything in a () block, so $return can be used to terminate
   early.  Only bind $var inside $body."
  (let ((body-name (gensym)))
    `(block nil
       (flet ((,body-name (,var)
		,@body))
	 (%enumerate-search-list ,pathname #',body-name)
	 ,result))))

(defun %enumerate-search-list (pathname function)
  (let* ((pathname (if (logical-pathname-p pathname)
		       (translate-logical-pathname pathname)
		       pathname))
	 (search-list (extract-search-list pathname nil)))
    (cond
     ((fi search-list)
      (funcall function pathname))
     ((fi (search-list-defined search-list))
      (if (probe-file (concatenate 'string
				   (search-list-name (extract-search-list pathname ()))
				   ":"))
	  (funcall function pathname)
	  (error "Undefined search list: ~A"
		 (search-list-name search-list))))
     (t
      (let ((tail (cddr (pathname-directory pathname))))
	(dolist (expansion
		 (search-list-expansions search-list))
	  (%enumerate-search-list (make-pathname :defaults pathname
						 :directory
						 (cons :absolute
						       (append expansion
							       tail)))
				  function)))))))


;;;;  Logical pathname support. ANSI 92-102 specification.
;;;;  As logical-pathname translations are loaded they are canonicalized as
;;;;  patterns to enable rapid efficent translation into physical pathnames.

;;;; Utilities.

;;; LOGICAL-WORD-OR-LOSE  --  Internal
;;;
;;; Canonicalize a logical pathanme word by uppercasing it checking that it
;;; contains only legal characters.
;;;
(defun logical-word-or-lose (word)
  (declare (string word))
  (let ((word (string-upcase word)))
    (dotimes (i (length word))
      (let ((ch (schar word i)))
	(unless (or (alpha-char-p ch) (digit-char-p ch) (char= ch #\-))
	  (error 'namestring-parse-error
		 :complaint "Logical namestring character ~
			     is not alphanumeric or hyphen:~%  ~S"
		 :arguments (list ch)
		 :namestring word :offset i))))
    word))

;;; FIND-LOGICAL-HOST  --  Internal
;;;
;;; Given a logical host or string, return a logical host.  If Error-p is
;;; NIL, then return NIL when no such host exists.
;;;
(defun find-logical-host (thing &optional (errorp t))
  (etypecase thing
    (string
     (let ((found (gethash (logical-word-or-lose thing)
			   *logical-hosts*)))
       (if (or found (not errorp))
	   found
	   (error 'simple-file-error
		  :pathname thing
		  :format-control "Logical host not yet defined: ~S"
		  :format-arguments (list thing)))))
    (logical-host thing)))

;;; INTERN-LOGICAL-HOST -- Internal
;;;
;;; Given a logical host name or host, return a logical host, creating a
;;; new one if necessary.
;;;
(defun intern-logical-host (thing)
  (declare (values logical-host))
  (or (find-logical-host thing nil)
      (let* ((name (logical-word-or-lose thing))
	     (new (make-logical-host :name name)))
	(setf (gethash name *logical-hosts*) new)
	new)))


;;;; Logical pathname parsing:

;;; MAYBE-MAKE-LOGICAL-PATTERN -- Internal
;;;
;;; Deal with multi-char wildcards in a logical pathname token.
;;;
(defun maybe-make-logical-pattern (namestring chunks)
  (let ((chunk (caar chunks)))
    (collect ((pattern))
      (let ((last-pos 0)
	    (len (length chunk)))
	(declare (fixnum last-pos))
	(loop
	  (when (= last-pos len) (return))
	  (let ((pos (or (position #\* chunk :start last-pos) len)))
	    (if (= pos last-pos)
		(when (pattern)
		  (error 'namestring-parse-error
			 :complaint "Double asterisk inside of logical ~
				     word: ~S"
			 :arguments (list chunk)
			 :namestring namestring
			 :offset (+ (cdar chunks) pos)))
		(pattern (subseq chunk last-pos pos)))
	    (if (= pos len)
		(return)
		(pattern :multi-char-wild))
	    (setq last-pos (1+ pos)))))
	(assert (pattern))
	(if (cdr (pattern))
	    (make-pattern (pattern))
	    (let ((x (car (pattern))))
	      (if (eq x :multi-char-wild)
		  :wild
		  x))))))

;;; LOGICAL-CHUNKIFY  --  Internal
;;;
;;; Return a list of conses where the cdr is the start position and the car
;;; is a string (token) or character (punctuation.)
;;;
(defun logical-chunkify (namestr start end)
  (collect ((chunks))
    (do ((i start (1+ i))
	 (prev 0))
	((= i end)
	 (when (> end prev)
	    (chunks (cons (nstring-upcase (subseq namestr prev end)) prev))))
      (let ((ch (schar namestr i)))
	(unless (or (alpha-char-p ch) (digit-char-p ch)
		    (member ch '(#\- #\*)))
	  (when (> i prev)
	    (chunks (cons (nstring-upcase (subseq namestr prev i)) prev)))
	  (setq prev (1+ i))
	  (unless (member ch '(#\; #\: #\.))
	    (error 'namestring-parse-error
		   :complaint "Illegal character for logical pathname:~%  ~S"
		   :arguments (list ch)
		   :namestring namestr
		   :offset i))
	  (chunks (cons ch i)))))
    (chunks)))

;;; PARSE-LOGICAL-NAMESTRING  -- Internal
;;;
;;; Break up a logical-namestring, always a string, into its constituent
;;; parts.
;;;
(defun parse-logical-namestring (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (collect ((directory))
    (let ((host nil)
	  (name nil)
	  (type nil)
	  (version nil))
      (labels ((expecting (what chunks)
		 (unless (and chunks (simple-string-p (caar chunks)))
		   (error 'namestring-parse-error
			  :complaint "Expecting ~A, got ~:[nothing~;~S~]."
			  :arguments (list what (caar chunks))
			  :namestring namestr
			  :offset (if chunks (cdar chunks) end)))
		 (caar chunks))
	       (parse-host (chunks)
		 (case (caadr chunks)
		   (#\:
		    (setq host
			  (find-logical-host (expecting "a host name" chunks)))
		    (parse-relative (cddr chunks)))
		   (t
		    (parse-relative chunks))))
	       (parse-relative (chunks)
		 (case (caar chunks)
		   (#\;
		    (directory :relative)
		    (parse-directory (cdr chunks)))
		   (t
		    (directory :absolute) ; Assumption! Maybe revoked later.
		    (parse-directory chunks))))
	       (parse-directory (chunks)
		 (case (caadr chunks)
		   (#\;
		    (directory
		     (let ((res (expecting "a directory name" chunks)))
		       (cond ((string= res "..") :up)
			     ((string= res "**") :wild-inferiors)
			     (t
			      (maybe-make-logical-pattern namestr chunks)))))
		    (parse-directory (cddr chunks)))
		   (t
		    (parse-name chunks))))
	       (parse-name (chunks)
		 (when chunks
		   (expecting "a file name" chunks)
		   (setq name (maybe-make-logical-pattern namestr chunks))
		   (expecting-dot (cdr chunks))))
	       (expecting-dot (chunks)
		 (when chunks
		   (unless (eql (caar chunks) #\.)
		     (error 'namestring-parse-error
			    :complaint "Expecting a dot, got ~S."
			    :arguments (list (caar chunks))
			    :namestring namestr
			    :offset (cdar chunks)))
		   (if type
		       (parse-version (cdr chunks))
		       (parse-type (cdr chunks)))))
	       (parse-type (chunks)
		 (expecting "a file type" chunks)
		 (setq type (maybe-make-logical-pattern namestr chunks))
		 (expecting-dot (cdr chunks)))
	       (parse-version (chunks)
		 (let ((str (expecting "a positive integer, * or NEWEST"
				       chunks)))
		   (cond
		    ((string= str "*") (setq version :wild))
		    ((string= str "NEWEST") (setq version :newest))
		    (t
		     (multiple-value-bind
			 (res pos)
			 (parse-integer str :junk-allowed t)
		       (unless (and res (plusp res))
			 (error 'namestring-parse-error
				:complaint "Expected a positive integer, ~
					    got ~S"
				:arguments (list str)
				:namestring namestr
				:offset (+ pos (cdar chunks))))
		       (setq version res)))))
		 (when (cdr chunks)
		   (error 'namestring-parse-error
			  :complaint "Extra stuff after end of file name."
			  :namestring namestr
			  :offset (cdadr chunks)))))
	(parse-host (logical-chunkify namestr start end)))
      (values host :unspecific
	      (fi (equal (directory) '(:absolute)) (directory))
	      name type version))))

;;; Can't defvar here because not all host methods are loaded yet.
;;;
(declaim (special *logical-pathname-defaults*))

;;; LOGICAL-PATHNAME -- Public
;;;
(defun logical-pathname (pathspec)
  "Converts the pathspec argument to a logical-pathname and returns it."
  (declare (type (or logical-pathname string stream) pathspec)
	   (values logical-pathname))
  (if (typep pathspec 'logical-pathname)
      pathspec
      (let ((res (parse-namestring pathspec nil *logical-pathname-defaults*)))
	(when (eq (%pathname-host res)
		  (%pathname-host *logical-pathname-defaults*))
	  (error "Logical namestring does not specify a host:~%  ~S"
		 pathspec))
	res)))


;;;; Logical pathname unparsing:

;;; UNPARSE-LOGICAL-DIRECTORY -- Internal
;;;
(defun unparse-logical-directory (pathname)
  (declare (type pathname pathname))
  (collect ((pieces))
    (let ((directory (%pathname-directory pathname)))
      (when directory
	(ecase (pop directory)
	  (:absolute)	 ;; Nothing special.
	  (:relative (pieces ";")))
	(dolist (dir directory)
	  (cond ((or (stringp dir) (pattern-p dir))
		 (pieces (unparse-logical-piece dir))
		 (pieces ";"))
		((eq dir :wild)
		 (pieces "*;"))
		((eq dir :wild-inferiors)
		 (pieces "**;"))
		(t
		 (error "Invalid directory component: ~S" dir))))))
    (apply #'concatenate 'simple-string (pieces))))

;;; UNPARSE-LOGICAL-PIECE -- Internal
;;;
(defun unparse-logical-piece (thing)
  (etypecase thing
    (simple-string thing)
    (pattern
     (collect ((strings))
       (dolist (piece (pattern-pieces thing))
	 (etypecase piece
	   (simple-string (strings piece))
	   (keyword
	    (cond ((eq piece :wild-inferiors)
		   (strings "**"))
		  ((eq piece :multi-char-wild)
		   (strings "*"))
		  (t (error "Invalid keyword: ~S" piece))))))
       (apply #'concatenate 'simple-string (strings))))))

;;; UNPARSE-ENOUGH-NAMESTRING -- Internal
;;;
(defun unparse-enough-namestring (pathname defaults)
  (let* ((path-dir (pathname-directory pathname))
        (def-dir (pathname-directory defaults))
        (enough-dir
         ;; Go down the directory lists to see what matches.  What's
         ;; left is what we want, more or less.
         (cond ((and (eq (first path-dir) (first def-dir))
                     (eq (first path-dir) :absolute))
                ;; Both paths are :absolute, so find where the common
                ;; parts end and return what's left
                (do* ((p (rest path-dir) (rest p))
                      (d (rest def-dir) (rest d)))
                     ((or (endp p) (endp d)
                          (not (equal (first p) (first d))))
                      `(:relative ,@p))))
               (t
                ;; At least one path is :relative, so just return the
                ;; original path.  If the original path is :relative,
                ;; then that's the right one.  If PATH-DIR is
                ;; :absolute, we want to return that except when
                ;; DEF-DIR is :absolute, as handled above. so return
                ;; the original directory.
                path-dir))))
    (make-pathname :host (pathname-host pathname)
                  :directory enough-dir
                  :name (pathname-name pathname)
                  :type (pathname-type pathname)
                  :version (pathname-version pathname))))

;;; UNPARSE-LOGICAL-NAMESTRING -- Internal
;;;
(defun unparse-logical-namestring (pathname)
  (declare (type logical-pathname pathname))
  (concatenate 'simple-string
	       (logical-host-name (%pathname-host pathname)) ":"
	       (unparse-logical-directory pathname)
	       (unparse-unix-file pathname)))


;;;; Logical pathname translations:

;;; CANONICALIZE-LOGICAL-PATHNAME-TRANSLATIONS -- Internal
;;;
;;; Verify that the list of translations consists of lists and prepare
;;; canonical translations (parse pathnames and expand out wildcards into
;;; patterns).
;;;
(defun canonicalize-logical-pathname-translations (transl-list host)
  (declare (type list transl-list) (type host host)
	   (values list))
  (collect ((res))
    (dolist (tr transl-list)
      (unless (and (consp tr) (= (length tr) 2))
	(error "Logical pathname translation is not a two-list:~%  ~S"
	       tr))
      (let ((from (first tr)))
	(res (list (if (typep from 'logical-pathname)
		       from
		       (parse-namestring from host))
		   (pathname (second tr))))))
    (res)))

;;; LOGICAL-PATHNAME-TRANSLATIONS -- Public
;;;
(defun logical-pathname-translations (host)
  "Return the (logical) host object argument's list of translations."
  (declare (type (or string logical-host) host)
	   (values list))
  (logical-host-translations (find-logical-host host)))

;;; (SETF LOGICAL-PATHNAME-TRANSLATIONS) -- Public
;;;
(defun (setf logical-pathname-translations) (translations host)
  "Set the translations list for the logical host argument.
   Return translations."
  (declare (type (or string logical-host) host)
	   (type list translations)
	   (values list))

  (let ((host (intern-logical-host host)))
    (setf (logical-host-canon-transls host)
	  (canonicalize-logical-pathname-translations translations host))
    (setf (logical-host-translations host) translations)))

;;; The search mechanism for loading pathname translations uses the CMUCL
;;; extension of search-lists.  The user can add to the library: search-list
;;; using setf.  The file for translations should have the name defined by
;;; the hostname (a string) and with type component "translations".

;;; LOAD-LOGICAL-PATHNAME-TRANSLATIONS -- Public
;;;
(defun load-logical-pathname-translations (host)
  "Search for a logical pathname named host, if not already defined. If already
   defined no attempt to find or load a definition is attempted and NIL is
   returned. If host is not already defined, but definition is found and loaded
   successfully, T is returned, else error."
  (declare (type string host)
	   (values (member t nil)))
  (unless (find-logical-host host nil)
    (with-open-file (in-str (make-pathname :defaults "library:"
					   :name host
					   :type "translations"))
      (if *load-verbose*
	  (format *error-output*
		  ";; Loading pathname translations from ~A~%"
		  (namestring (truename in-str))))
      (setf (logical-pathname-translations host) (read in-str)))
    t))

;;; TRANSLATE-LOGICAL-PATHNAME  -- Public
;;;
(defun translate-logical-pathname (pathname &key)
  "Translates pathname to a physical pathname, which is returned."
  (declare (type path-designator pathname)
	   (values (or null pathname)))
  (typecase pathname
    (logical-pathname
     (dolist (x (logical-host-canon-transls (%pathname-host pathname))
		(error 'simple-file-error
		       :pathname pathname
		       :format-control "No translation for ~S"
		       :format-arguments (list pathname)))
       (destructuring-bind (from to) x
	 (when (pathname-match-p pathname from)
	   (return (translate-logical-pathname
		    (translate-pathname pathname from to)))))))
    (pathname pathname)
    (stream (translate-logical-pathname (pathname pathname)))
    (t (translate-logical-pathname (logical-pathname pathname)))))

(defvar *logical-pathname-defaults*
  (%make-logical-pathname (make-logical-host :name "BOGUS") :unspecific
			  () () () ()))
