;;; This file contains definitions for the Line structure, and some
;;; functions and macros to manipulate them.
;;;
;;; This stuff was allowed to become implementation dependant because the
;;; editor makes thousands of lines, so speed is really important.  In some
;;; implementations (the Perq for example) it may be desirable to only
;;; actually cons the strings in the line objects when someone touches
;;; them, and just keep a pointer in the line to where the file is mapped
;;; in memory.  Such lines are called "buffered".  This stuff links up with
;;; the file-reading stuff and the line-image building stuff.

(in-package "EDI")

(export '(line linep line-previous line-next line-plist line-signature))

#[ Lines

In the editor all text is in some line.  Text is broken into lines wherever
it contains a newline character; newline characters are never stored, but
are assumed to exist between every pair of lines.  The implicit newline
character is treated as a single character by the text primitives.

{function:ed:linep}
{function:ed:line-string}
{function:ed:line-previous}
{function:ed:line-next}
{function:ed:line-buffer}
{function:ed:line-length}
{function:ed:line-character}
{function:ed:line-plist}
{function:ed:line-signature}
]#

(setf (documentation 'linep 'function)
  "Return true if $line is a line, else ().")
(setf (documentation 'line-previous 'function)
  "Return the previous line if there is one, else ().")
(setf (documentation 'line-next 'function)
  "Return the next line if there is one, else ().")
(setf (documentation 'line-plist 'function)
  "Return the property-list for line.

   `setf', `getf', `putf' and `remf' change properties.  This is typically
   used in conjunction with `line-signature' to cache information about the
   line's contents.")


;;;; The line object.

(proclaim '(inline %make-line))
(defstruct (line (:print-function %print-hline)
		 (:constructor %make-line)
		 (:predicate linep))
  "An editor line object."
  ;;
  ;; Something that represents the contents of the line.  This is
  ;; guaranteed to change (as compared by EQL) whenever the contents of the
  ;; line changes, but might at arbitarary other times.  There are
  ;; currently about three different cases:
  ;;
  ;; Normal:
  ;;    A simple string holding the contents of the line.
  ;;
  ;; A cached line:
  ;;    The line is eq to Open-Line, and the actual contents are in the
  ;;    line cache.  The %Chars may be either the original contents or a
  ;;    negative fixnum.
  ;;
  ;; A buffered line:
  ;;    The line hasn't been touched since it was read from a file, and the
  ;;    actual contents are in some system I/O area.  This is indicated by
  ;;    the Line-Buffered-P slot being true.  In buffered lines on the RT,
  ;;    the %Chars slot contains the system-area-pointer to the beginning
  ;;    of the characters.
  (%chars "")
  ;;
  ;; Pointers to the next and previous lines in the doubly linked list of
  ;; line structures.
  previous
  next
  ;;
  ;; A list of all the permanent marks pointing into this line.
  (marks ())
  ;;
  ;; The buffer to which this line belongs, or a *disembodied-buffer-count*
  ;; if the line is not in any buffer.
  %buffer
  ;;
  ;; A non-negative integer (fixnum) that represents the ordering of lines
  ;; within continguous range of lines (a buffer or disembuffered region).
  ;; The number of the Line-Next is guaranteed to be strictly greater than
  ;; our number, and the Line-Previous is guaranteed to be strictly less.
  (number 0)
  ;;
  ;; The line property list, used by user code to annotate the text.
  plist
  ;;
  ;; A slot that indicates whether this line is a buffered line, and if so
  ;; contains information about how the text is stored.  On the RT, this is
  ;; the length of the text pointed to by the Line-%Chars.
  #+Buffered-Lines
  (buffered-p ()))

;;; Make Line-Chars the same as Line-%Chars on implementations without
;;; buffered lines.
;;;
#-Buffered-Lines
(defmacro line-chars (x)
  `(line-%chars ,x))

;;; If buffered lines are supported, then we create the string
;;; representation for the characters when someone uses Line-Chars.  People
;;; who are prepared to handle buffered lines or who just want a signature
;;; for the contents can use Line-%chars directly.
;;;
#+Buffered-Lines
(defmacro line-chars (line)
  `(the simple-string (if (line-buffered-p ,line)
			  (read-buffered-line ,line)
			  (line-%chars ,line))))
;;;
#+Buffered-Lines
(defsetf line-chars %set-line-chars)
;;;
#+Buffered-Lines
(defmacro %set-line-chars (line chars)
  `(setf (line-%chars ,line) ,chars))

;;; Line-Signature  --  Public
;;;
;;; We can just return the Line-%Chars.
;;;
(proclaim '(inline line-signature))
(defun line-signature (line)
  "Return a signature for the contents of $line, guaranteeing that any
   modification of the text on $line will result in the signature changing
   so that it fails to be `eql' to any previous value.  The signature may
   change even when the text remains the same, but this happens seldom."
  (line-%chars line))

;;; Return a copy of Line in buffer Buffer with the same chars.  We use
;;; this macro where we want to copy a line because it takes care of
;;; the case where the line is buffered.
;;;
(defmacro %copy-line (line &key previous number %buffer)
  `(make-line :chars (line-%chars ,line)
	      :previous ,previous
	      :number ,number
	      :%buffer ,%buffer
	      #+Buffered-Lines :buffered-p
	      #+Buffered-Lines (line-buffered-p ,line)))

;;; Hide the fact that the slot isn't really called CHARS.
;;;
(defmacro make-line (&rest keys)
  `(%make-line ,@(substitute :%chars :chars keys)))

(defmacro line-length* (line)
  "Return the number of characters on $line."
  `(cond ((eq ,line open-line)
	  (+ left-open-pos (- line-cache-length right-open-pos)))
	 ((line-buffered-p ,line))
	 (t
	  (length (the simple-string (line-%chars ,line))))))
