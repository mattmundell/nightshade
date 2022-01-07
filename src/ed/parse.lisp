;;; Support for buffer parsers.

(in-package "ED")


;;;; Interface definitions.

(defun parse (buffer-parser)
  (let* ((parse::*mark* (copy-mark (current-point)))
	 (parse::*marks* `((,parse::*mark*))))
    ; FIX perhaps after parse kill buffers assoc w marks
    ;	  (first-mark parse::*marks*))
    (prog1 (funcall buffer-parser)
      (delete-mark parse::*mark*))))


;;;; Helper for access to EDI package.

(defun call-string-to-region (string)
  (string-to-region string))


;;;; Function predefined for the parser.

(defun copy-marks (marks)
  "Return a copy of the list Marks, updating the marks in the cdrs of the
   list elements to copies of the cars."
  (mapcar (lambda (l) (list (car l) (copy-mark (car l)))) marks))

(defun free-marks (marks)
  "Calls delete-mark on the marks in the cdrs of Marks."
  (mapcar (lambda (l) (delete-mark (cadr l))) marks))

(defun recover-marks (marks branch-marks)
  "Recover parse::*marks* and parse::*mark* after a failed \"many\" or \"or\" branch.
   parse::*marks* is recovered to be like Branch-marks.  Marks is the original
   (list) position of Branch-marks in the list parse::*marks*.  Branch-marks is a
   copy of parse::*marks* from that position made at the branch, with a copy of
   the mark in the car of each element in the element cdr."
  (let ((current (caar parse::*marks*)))
    (do* ((marks marks (cdr marks))
	  (m (caar marks) (caar marks)))
	 ((eq marks nil)
	  (error "Reached end of marks."))
      (let ((branch-pair (assoc m branch-marks)))
	(if branch-pair
	    (move-mark m (cadr branch-pair))
	    (progn
	      (care-pop marks)
	      (delete-buffer (line-buffer (mark-line m)))
	      ;; (delete-mark m) FIX
	      )))
      (when (eq m current)
	(setq parse::*marks* marks)
	(setq parse::*mark* (caar parse::*marks*))
	(return-from nil)))))

(defun buffer-parser-read-char ()
  "Return the next character at parse::*mark*, moving mark forward one character.
   If at the end of the buffer then pop a mark from parse::*marks* into parse::*mark*,
   and retry.  If at the end of then buffer then switch to the next mark in
   parse::*marks*, and retry."
  (if (mark-after parse::*mark*)
      (previous-character parse::*mark*)
      (progn
	(when (cdr parse::*marks*)
	  (setq parse::*marks* (cdr parse::*marks*))
	  (setq parse::*mark* (car parse::*marks*))
	  (buffer-parser-read-char)))))

(declaim (inline buffer-parser-read-and-compare-char))

(defun buffer-parser-read-and-compare-char (char)
  "Read the next character at parse::*mark*, moving mark forward one character.
   Return t if the character is Char, else nil.  If at the end of the
   buffer then switch to the next mark in parse::*marks*, and retry."
  (if (mark-after parse::*mark*)
      (eq (previous-character parse::*mark*) char)
      (progn
	(when (cdr parse::*marks*)
	  (setq parse::*marks* (cdr parse::*marks*))
	  (setq parse::*mark* (car parse::*marks*))
	  (buffer-parser-read-and-compare-char-1 char)))))

(defun buffer-parser-read-and-compare-char-1 (char)
  "Recursive buffer-parser-read-and-compare-char."
  (if (mark-after parse::*mark*)
      (eq (previous-character parse::*mark*) char)
      (progn
	(when (cdr parse::*marks*)
	  (setq parse::*marks* (cdr parse::*marks*))
	  (setq parse::*mark* (car parse::*marks*))
	  (buffer-parser-read-and-compare-char-1 char)))))

(declaim (inline buffer-parser-read-and-fcompare-char))

(defun buffer-parser-read-and-fcompare-char (char)
  "Read the next character at parse::*mark*, moving mark forward one character.
   Return t if the character is upper or lower case Char, else nil.  If at
   the end of the buffer then switch to the next mark in parse::*marks*, and
   retry."
  (if (mark-after parse::*mark*)
      (eq (char-upcase (previous-character parse::*mark*)) (char-upcase char))
      (progn
	(when (cdr parse::*marks*)
	  (setq parse::*marks* (cdr parse::*marks*))
	  (setq parse::*mark* (car parse::*marks*))
	  (buffer-parser-read-and-fcompare-char-1 char)))))

(defun buffer-parser-read-and-fcompare-char-1 (char)
  "Recursive buffer-parser-read-and-compare-char."
  (if (mark-after parse::*mark*)
      (eq (char-upcase (previous-character parse::*mark*)) (char-upcase char))
      (when (cdr parse::*marks*)
	(setq parse::*marks* (cdr parse::*marks*))
	(setq parse::*mark* (car parse::*marks*))
	(buffer-parser-read-and-fcompare-char-1 char))))

(defun buffer-parse-alphanumeric (&optional next previous parent)
  "Read next character at parse::*mark*.  If an alphabet or numeric character is
   read return two values both the same node of the character, otherwise
   return nil."
  (let ((ch (buffer-parser-read-char)))
    (if (and ch (alphanumericp ch))
	(let ((node (parse::make-char-node :content ch ; FIX ::
					  :next next
					  :previous previous
					  :parent parent)))
	  (if previous (setf (parse:node-next previous) node))
	  (values node node)))))

(defun group-buffer-parse-alphanumeric (region)
  "Read next character at parse::*mark*.  If an alphabet or numeric
   character is read append the character to array $region and return the
   character, otherwise return nil."
  (let ((ch (buffer-parser-read-char)))
    (when (and ch (alphanumericp ch))
      (with-output-to-string (stream region)
	(write-char ch stream))
      ch)))

(defun buffer-parse-char (&optional next previous parent)
  "Read next character at parse::*mark*.  If the read succeeds return two values
   both the same node of the read character, otherwise return nil."
  (let ((char (buffer-parser-read-char)))
    (when char
      (let ((node (parse::make-char-node :content char  ; FIX ::
				  :next next
				  :previous previous
				  :parent parent)))
	(values node node)))))

(declaim (inline buffer-parse-c))

(defun buffer-parse-c (&optional next previous parent)
  "Call `buffer-parse-char' on the arguments."
  (buffer-parse-char next previous parent))

(defun buffer-group-parse-char (region)
  "Read next character at parse::*mark*.  If the read succeeds append the
   character to array $region and return the character, otherwise return
   nil."
  (let ((ch (buffer-parser-read-char)))
    (when ch
      (with-output-to-string (stream region)
	(write-char ch stream))
      ch)))

(defun primitive-buffer-parse-char (char &optional next previous parent)
  "Read next character at parse::*mark*.  Return a node of Char if Char was read,
   otherwise return ()."
  (when (buffer-parser-read-and-compare-char char)
    (let ((node (parse::make-char-node :content char ; FIX ::
				      :next next
				      :previous previous
				      :parent parent)))
      (if previous (setf (parse:node-next previous) node))
      (values node node))))

(defun primitive-buffer-fparse-char (char &optional next previous parent)
  "Read next character at parse::*mark*.  Return a node of Char if upper or lower
   case Char was read, otherwise return nil."
  (when (buffer-parser-read-and-fcompare-char char)
    (let ((node (parse::make-char-node :content char ; FIX ::
				      :next next
				      :previous previous
				      :parent parent)))
      (if previous (setf (parse:node-next previous) node))
      (values node node))))

(defun primitive-group-buffer-parse-char (char region)
  "Read next character at parse::*mark*.  Append $char to array $region and
   return $char if $char was read, otherwise return nil."
  (when (buffer-parser-read-and-compare-char char)
    (with-output-to-string (stream region)
      (write-char char stream))
    char))

(defun primitive-group-buffer-fparse-char (char region)
  "Read next character at parse::*mark*.  Append $char to array $region and
   return $char if upper or lower case $char was read, otherwise return
   nil."
  (when (buffer-parser-read-and-fcompare-char char)
    (with-output-to-string (stream region)
      (write-char char stream))
    char))

(defun primitive-buffer-parse-string (string &optional next previous parent)
  "Read String from parse::*mark*.  Return a node of the parsed string if
   successful, else return nil."
  (dotimes (pos (length string))
    (or (buffer-parser-read-and-compare-char (char string pos))
	(return-from primitive-buffer-parse-string nil)))
  (let ((node (parse::make-region-node :content string ; FIX ::
				      :next next
				      :previous previous
				      :parent parent)))
    (if previous (setf (parse:node-next previous) node))
    (values node node)))

#|
(defun primitive-buffer-parse-string (string &optional next previous parent)
  "Read String from parse::*mark*.  Return a node of the parsed string if
   successful, else return nil."
  (dotimes (pos (length string))
; FIX how much does check for parse::*marks* cost?
;    (or (buffer-parser-read-and-compare-char (char string pos))
    (mark-after parse::*mark*)
    (or (eq (next-character parse::*mark*) (char string pos))
	(return-from primitive-buffer-parse-string nil)))
  (let ((node (parse::make-region-node :content string ; FIX ::
				      :next next
				      :previous previous
				      :parent parent)))
    (when previous
      (setf (parse:node-next previous) node))
    (values node node)))
|#

(defun primitive-buffer-fparse-string (string &optional next previous parent)
  "Read String from parse::*mark* with case folding.  Return a node of String if
   successful, else return nil."
  (let ((mark parse::*mark*))
    (dotimes (pos (length string))
      (or (buffer-parser-read-and-fcompare-char (char string pos))
	  (return-from primitive-buffer-fparse-string nil)))
    (let ((node (parse::make-region-node :content (region-to-string ; FIX ::
						  (region mark parse::*mark*))
					:next next
					:previous previous
					:parent parent)))
      (if previous (setf (parse:node-next previous) node))
      (values node node))))

(defun primitive-group-buffer-parse-string (string region)
  "Read String from parse::*mark*.  If successful append $string to array
   $region and return $string, otherwise return nil."
  (dotimes (pos (length string))
    (or (buffer-parser-read-and-compare-char (char string pos))
	(return-from primitive-group-buffer-parse-string nil)))
  (with-output-to-string (stream region)
    (write-string string stream))
  string)

(defun primitive-group-buffer-fparse-string (string region)
  "Read $string from parse::*mark* with case folding.  If successful append
   $string to $region and return $string, otherwise return nil."
  (dotimes (pos (length string))
    (or (buffer-parser-read-and-compare-char (char string pos))
	(return-from primitive-group-buffer-fparse-string nil)))
  (with-output-to-string (stream region)
    (write-string string stream))
  string)



;;; FIX these duplicate the ones in the "parse" pkg because defparser
;;; generates calls to these functions in the pkg in which defparser is
;;; called.  so any package that uses :alpha :c or :char in a rule needs to
;;; define these.  a solution may be to make defparser generates calls to
;;; parse:parse-alphanumeric as special cases when encountering :alph,c,char

;;; Public
;;;
(defun parser-read-char ()
  "Read and return the next character in parse:*stream*.  If the end of stream
   has already been reached then pop a stream from parse:*streams* into parse:*stream*,
   and retry."
  (let ((ch (read-char parse:*stream* nil :eof)))
    (if (eq ch :eof)
	(progn
	  (when (cdr parse:*streams*)
	    (setq parse:*streams* (cdr parse:*streams*))
	    (setq parse:*stream* (caar parse:*streams*))
	    (parser-read-char)))
	ch)))

(declaim (inline parser-read-and-compare-char))

(defun parser-read-and-compare-char (char)
  "Read the next character in parse:*stream*, returning t if it is Char else
   false.  If the end of stream has already been reached then pop a stream
   from parse:streams into parse:*stream*, and retry."
  (let ((ch (read-char parse:*stream* nil :eof)))
    (or (eq ch char)
	(when (and (eq ch :eof)
		   (cdr parse:*streams*))
	  (setq parse:*streams* (cdr parse:*streams*))
	  (setq parse:*stream* (caar parse:*streams*))
	  (parser-read-and-compare-char-1 char)))))

(defun parser-read-and-compare-char-1 (char)
  "Recursive parser-read-and-compare-char-1."
  (let ((ch (read-char parse:*stream* nil :eof)))
    (or (eq ch char)
	(when (and (eq ch :eof)
		   (cdr parse:*streams*))
	  (setq parse:*streams* (cdr parse:*streams*))
	  (setq parse:*stream* (caar parse:*streams*))
	  (parser-read-and-compare-char-1 char)))))

(declaim (inline parser-read-and-fcompare-char))

(defun parser-read-and-fcompare-char (char)
  "Read the next character in parse:*stream*, returning t if it is upper or lower
   case Char else false.  If the end of stream has already been reached
   then pop a stream from parse:*streams* into parse:*stream*, and retry."
  (let ((ch (read-char parse:*stream* nil nil)))
    (if ch
	(eq (char-upcase ch) (char-upcase char))
	(when (cdr parse:*streams*)
	  (setq parse:*streams* (cdr parse:*streams*))
	  (setq parse:*stream* (caar parse:*streams*))
	  (parser-read-and-fcompare-char-1 char)))))

(defun parser-read-and-fcompare-char-1 (char)
  "Recursive parser-read-and-compare-char-1."
  (let ((ch (read-char parse:*stream* nil nil)))
    (if ch
	(eq (char-upcase ch) (char-upcase char))
	(when (cdr parse:*streams*)
	  (setq parse:*streams* (cdr parse:*streams*))
	  (setq parse:*stream* (caar parse:*streams*))
	  (parser-read-and-fcompare-char-1 char)))))

;;; Public
;;;
(defun parse-alphanumeric (&optional next previous parent)
  "Read next character in parse:*stream*.  If an alphabet or numeric character is
   read return two values both the same node of the character, otherwise
   return nil."
  (let ((ch (parser-read-char)))
    (if (and ch (alphanumericp ch))
	(let ((node (make-char-node :content ch
				    :next next
				    :previous previous
				    :parent parent)))
	  (when previous
	    (setf (node-next previous) node))
	  (values node node)))))

;;; Public
;;;
(defun group-parse-alphanumeric (region)
  "Read next character in parse:*stream*.  If an alphabet or numeric character is
   read then append the character to array $region and return the
   character, otherwise return nil."
  (let ((ch (parser-read-char)))
    (when (and ch (alphanumericp ch))
      (with-output-to-string (stream region)
	(write-char ch stream))
      ch)))

(defun parse-char (&optional next previous parent)
  "Read next character in parse:*stream*.  If the read succeeds return two values
   both the same node of the read character, otherwise return nil."
  (let ((char (parser-read-char)))
    (when char
      (let ((node (make-char-node :content char
				  :next next
				  :previous previous
				  :parent parent)))
	(values node node)))))

(declaim (inline parse-c))

(defun parse-c (&optional next previous parent)
  "Call `parse-char' on the arguments."
  (parse-char next previous parent))

;; FIX the group function are surely slowed by each calling
;;     with-output-to-string.

;;; Public
;;;
(defun group-parse-char (region)
  "Read next character in parse:*stream*.  If the read succeeds append the
   character to array $region and return the character, otherwise return
   nil."
  (let ((ch (parser-read-char)))
    (when ch
      (with-output-to-string (stream region)
	(write-char ch stream))
      ch)))

