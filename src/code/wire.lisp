;;; An interface to internet domain sockets.

(in-package "WIRE")

(export '(remote-object-p remote-object
	  remote-object-local-p remote-object-eq
	  remote-object-value make-remote-object forget-remote-translation
	  make-wire wire-p wire-fd wire-listen wire-get-byte wire-get-number
	  wire-get-string wire-get-object wire-force-output wire-output-byte
	  wire-output-number wire-output-string wire-output-object
	  wire-output-funcall wire-error wire-eof wire-io-error
	  *current-wire* wire-get-bignum wire-output-bignum))


#[ The WIRE Package

The wire package provides for sending data along wires.  The remote package
sits on top of this package.  All data sent with a given output routine
must be read in the remote process with the complementary fetching routine.
For example, if you send so a string with `wire-output-string', the remote
process must know to use `wire-get-string'.  To avoid rigid data transfers
and complicated code, the interface supports sending tagged data.  With
tagged data, the system sends a tag announcing the type of the next data,
and the remote system takes care of fetching the appropriate type.

When using interfaces at the wire level instead of the RPC level,
the remote process must read everything sent by these routines.  If
the remote process leaves any input on the wire, it will later
mistake the data for an RPC request causing unknown lossage.

[ Untagged Data ]
[ Tagged Data   ]
[ Making Wires  ]
]#


#[ Untagged Data

When using these routines both ends of the wire know exactly what types are
coming and going and in what order. This data is restricted to the following
types:

 * 8 bit unsigned bytes.

 * 32 bit unsigned bytes.

 * 32 bit integers.

 * simple-strings less than 65535 in length.

The following functions either output or input an object of the specified
data type.  When any of these output routines are used to send data across
the wire, the corresponding input routine must be used to interpret the
data.

{function:wire:wire-output-byte}
{function:wire:wire-get-byte}
{function:wire:wire-output-number}
{function:wire:wire-get-number}
{function:wire:wire-output-string}
{function:wire:wire-get-string}
]#


#[ Tagged Data

When using these routines, the system automatically transmits and interprets
the tags, so both ends can figure out what kind of data transfers
occur.  Sending tagged data allows a greater variety of data types: integers
inclusively less than 32 bits in length, symbols, lists, and remote-objects
([Remote Objects]).  The system sends symbols as two strings, the
package name and the symbol name.  The package must exist remotely,
else the remote process signals an error.  FIX The system ignores other slots of
symbols.  Lists may be any tree of the above valid data types.  To send other
data types you must represent them in terms of these supported types.  For
example, you could use `prin1-to-string' locally, send the string, and use
`read-from-string' remotely.

{function:wire:wire-output-object}
{function:wire:wire-get-object}
]#


(eval-when (compile load eval) ; For macros in remote.lisp.

(defconstant buffer-size 2048)

(defconstant initial-cache-size 16)

(defconstant funcall0-op 0)
(defconstant funcall1-op 1)
(defconstant funcall2-op 2)
(defconstant funcall3-op 3)
(defconstant funcall4-op 4)
(defconstant funcall5-op 5)
(defconstant funcall-op 6)
(defconstant number-op 7)
(defconstant string-op 8)
(defconstant symbol-op 9)
(defconstant save-op 10)
(defconstant lookup-op 11)
(defconstant remote-op 12)
(defconstant cons-op 13)
(defconstant bignum-op 14)

) ;eval-when


;;;; Structure.

#[ Making Wires

It is possible to manually create wires, in addition to the remote
package's interface creating them for you.  A Unix file descriptor is
needed to create a wire.  Unix file descriptors are described in section 2
of the Unix manual pages.

{function:wire:make-wire}
{function:wire:wire-p}
{function:wire:wire-fd}
]#

(defvar *current-wire* nil
  "The wire the form we are currently evaluating came across.")

(defvar *this-host* nil
  "Unique identifier for this host.")
(defvar *this-pid* nil
  "Unique identifier for this process.")

(defvar *object-to-id* (make-hash-table :test 'eq)
  "Hash table mapping local objects to the corresponding remote id.")
(defvar *id-to-object* (make-hash-table :test 'eql)
  "Hash table mapping remote id's to the curresponding local object.")
(defvar *next-id* 0
  "Next available id for remote objects.")

(defstruct (wire
            (:constructor make-wire (fd))
            (:print-function
             (lambda (wire stream depth)
               (declare (ignore depth))
               (format stream
                       "#<wire fd=~a>"
		       (wire-fd wire)))))
  fd

  (ibuf (make-string buffer-size))
  (ibuf-offset 0)
  (ibuf-end 0)
  (object-cache (make-array initial-cache-size))

  (obuf (make-string buffer-size))
  (obuf-end 0)
  (cache-index 0)
  (object-hash (make-hash-table :test 'eq)))

(setf (documentation 'make-wire 'function)
  "Create a new wire when supplied with the file descriptor to use for the
   underlying I/O operations.")

(setf (documentation 'wire-p 'function)
  "Return t if $wire is a wire, else ().")

(setf (documentation 'wire-fd 'function)
  "Return the file descriptor used by $wire.")

(defstruct (remote-object
	    (:constructor %make-remote-object (host pid id))
	    (:print-function
	     (lambda (obj stream depth)
	       (declare (ignore depth))
	       (format stream "#<Remote Object: [~x:~a] ~s>"
		       (remote-object-host obj)
		       (remote-object-pid obj)
		       (remote-object-id obj)))))
  host
  pid
  id)

(setf (documentation 'remote-object-p 'function)
  "Return t if $x is a remote object, else ().")

(define-condition wire-error (error)
  ((wire :reader wire-error-wire :initarg :wire))
  (:report (lambda (condition stream)
	     (format stream "There is a problem with ~A."
		     (wire-error-wire condition)))))

(define-condition wire-eof (wire-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Recieved EOF on ~A."
		     (wire-error-wire condition)))))

(define-condition wire-io-error (wire-error)
  ((when :reader wire-io-error-when :initarg :when :initform "using")
   (msg :reader wire-io-error-msg :initarg :msg :initform "Failed."))
  (:report (lambda (condition stream)
	     (format stream "Error ~A ~A: ~A."
		     (wire-io-error-when condition)
		     (wire-error-wire condition)
		     (wire-io-error-msg condition)))))


;;;; Remote Objects.

;;; REMOTE-OBJECT-LOCAL-P -- public
;;;
;;; First, make sure the *this-host* and *this-pid* are set. Then test to
;;; see if the remote object's host and pid fields are *this-host* and
;;; *this-pid*
;;;
(defun remote-object-local-p (remote)
  "Return t if the remote object $remote is defined locally.  This can only
   occur if the local process created $remote with `make-remote-object'."
  (declare (type remote-object remote))
  (unless *this-host*
    (setf *this-host* (unix:unix-gethostid))
    (setf *this-pid* (unix:unix-getpid)))
  (and (eql (remote-object-host remote) *this-host*)
       (eql (remote-object-pid remote) *this-pid*)))

;;; REMOTE-OBJECT-EQ -- public
;;;
;;; Remote objects are considered EQ if they refer to the same object, ie
;;; Their host, pid, and id fields are the same (eql, cause they are all
;;; numbers).
;;;
(defun remote-object-eq (remote1 remote2)
  "Return t if $remote1 and $remote2 refer to the same (`eq') Lisp object
   in the same process."
  (declare (type remote-object remote1 remote2))
  (and (eql (remote-object-host remote1)
	    (remote-object-host remote2))
       (eql (remote-object-pid remote1)
	    (remote-object-pid remote2))
       (eql (remote-object-id remote1)
	    (remote-object-id remote2))))

;;; REMOTE-OBJECT-VALUE --- public
;;;
;;; First assure that the remote object is defined locally. If so, look up
;;; the id in *id-to-objects*.  table. This will only happen if
;;; FORGET-REMOTE-TRANSLATION has been called on the local object.
;;;
(defun remote-object-value (remote)
  "Return the original object used to create the given remote object.
   Throw an error if some other process originally created the
   remote-object or if `forget-remote-translation' has been called on this
   remote object."
  (declare (type remote-object remote))
  (or (remote-object-local-p remote)
      (error "~S is defined is a different process." remote))
  (multiple-value-bind
      (value found)
      (gethash (remote-object-id remote)
	       *id-to-object*)
    (or found
	(cerror
	 "Use the value of ()"
	 "No value for ~S -- FORGET-REMOTE-TRANSLATION was called to early."
	 remote))
    value))

;;; MAKE-REMOTE-OBJECT --- public
;;;
;;; Convert the given local object to a remote object. If the local object
;;; is alread entered in the *object-to-id* hash table, just use the old
;;; id.  Otherwise, grab the next id and put add both mappings to the two
;;; hash tables.
;;;
(defun make-remote-object (local)
  "Return a remote-object that has $local as its value.  The remote-object
   can be passed across wires just like the directly supported wire data
   types."
  (unless *this-host*
    (setf *this-host* (unix:unix-gethostid))
    (setf *this-pid* (unix:unix-getpid)))
  (let ((id (gethash local *object-to-id*)))
    (unless id
      (setf id *next-id*)
      (setf (gethash local *object-to-id*) id)
      (setf (gethash id *id-to-object*) local)
      (incf *next-id*))
    (%make-remote-object *this-host* *this-pid* id)))

;;; FORGET-REMOTE-TRANSLATION -- public
;;;
;;; Remove any translation information about the given object. If there is
;;; currenlt no translation for the object, don't bother doing anything.
;;; Otherwise remove it from the *object-to-id* hashtable, and remove the
;;; id from the *id-to-object* hashtable.
;;;
(defun forget-remote-translation (local)
  "Update $local so that GC can reclaim the associated storage.

   This should be used after the last reference $local.  If some remote
   process does send a reference to $local, `remote-object-value' signals
   an error."
  (let ((id (gethash local *object-to-id*)))
    (when id
      (remhash local *object-to-id*)
      (remhash id *id-to-object*)))
  (values))


;;;; Wire input routines.

;;; WIRE-LISTEN -- public
;;;
;;; If nothing is in the current input buffer, select on the file
;;; descriptor.
;;;
(defun wire-listen (wire)
  "Return T iff anything is in the input buffer or available on the socket."
  (or (< (wire-ibuf-offset wire)
	 (wire-ibuf-end wire))
      (multiple-value-bind
	  (number error)
	  (unix:unix-select (1+ (wire-fd wire))
			    (ash 1 (wire-fd wire))
			    0
			    0
			    0)
	(unless number
	  (error 'wire-io-error
		 :wire wire
		 :when "listening to"
		 :msg (unix:get-unix-error-msg error)))
	(not (zerop number)))))

;;; FILL-INPUT-BUFFER -- Internal
;;;
;;; Fill the input buffer from the socket. If we get an error reading,
;;; signal a wire-io-error. If we get an EOF, signal a wire-eof error. If
;;; we get any data, set the ibuf-end index.
;;;
(defun fill-input-buffer (wire)
  "Read data off the socket, filling the input buffer. The buffer is
   cleared first. If fill-input-buffer returns, it is guarenteed that there
   will be at least one byte in the input buffer. If EOF was reached, as
   wire-eof error is signaled."
  (setf (wire-ibuf-offset wire) 0
	(wire-ibuf-end wire) 0)
  (let ((fd (wire-fd wire))
	(ibuf (wire-ibuf wire)))
    (or ibuf (error 'wire-eof :wire wire))

    (multiple-value-bind
	(bytes error)
	(system:without-gcing
	 (unix:unix-read fd (system:vector-sap ibuf) buffer-size))
      (cond ((null bytes)
	     (error 'wire-io-error
		    :wire wire
		    :when "reading"
		    :msg (unix:get-unix-error-msg error)))
	    ((zerop bytes)
	     (setf (wire-ibuf wire) nil)
	     (error 'wire-eof :wire wire))
	    (t
	     (setf (wire-ibuf-end wire) bytes)))))
  (values))

;;; WIRE-GET-BYTE -- public
;;;
;;; Check to see if there is anything in the input buffer. If not, use
;;; FILL-INPUT-BUFFER to get something. Return the next byte, adjusting the
;;; input offset index.
;;;
(defun wire-get-byte (wire)
  "Return the next byte from $wire."
  (when (<= (wire-ibuf-end wire)
	    (wire-ibuf-offset wire))
    (fill-input-buffer wire))
  (prog1
      (char-int (schar (wire-ibuf wire)
		       (wire-ibuf-offset wire)))
    (incf (wire-ibuf-offset wire))))

;;; WIRE-GET-NUMBER -- public
;;;
;;; Just read four bytes and pack them together with normal math ops.
;;;
(defun wire-get-number (wire &optional (signed t))
  "Read a number off $wire.  Numbers are 4 bytes in network order.  If
   $signed is true then consider the number signed."
  (let* ((b1 (wire-get-byte wire))
	 (b2 (wire-get-byte wire))
	 (b3 (wire-get-byte wire))
	 (b4 (wire-get-byte wire))
	 (unsigned
	  (+ b4 (* 256 (+ b3 (* 256 (+ b2 (* 256 b1))))))))
    (if (and signed (> b1 127))
	(logior (ash -1 32) unsigned)
	unsigned)))

;;; WIRE-GET-BIGNUM -- public
;;;
;;; Extracts a number, which might be a bignum.
;;;
(defun wire-get-bignum (wire)
  "Reads an arbitrary integer sent by WIRE-OUTPUT-BIGNUM from the wire and
   return it."
  (let ((count-and-sign (wire-get-number wire)))
    (do ((count (abs count-and-sign) (1- count))
	 (result 0 (+ (ash result 32) (wire-get-number wire nil))))
	((not (plusp count))
	 (if (minusp count-and-sign)
	     (- result)
	     result)))))

;;; WIRE-GET-STRING -- public
;;;
;;; Use WIRE-GET-NUMBER to read the length, then keep pulling stuff out of
;;; the input buffer and re-filling it with FILL-INPUT-BUFFER until we've
;;; read the entire string.
;;;
(defun wire-get-string (wire)
  "Read a string from $wire.  The first four bytes spec the size."
  (let* ((length (wire-get-number wire))
	 (result (make-string length))
	 (offset 0)
	 (ibuf (wire-ibuf wire)))
    (declare (simple-string result ibuf)
	     (integer length offset))
    (loop
      (let ((avail (- (wire-ibuf-end wire)
		      (wire-ibuf-offset wire))))
	(declare (integer avail))
	(cond ((<= length avail)
	       (replace result
			ibuf
			:start1 offset
			:start2 (wire-ibuf-offset wire))
	       (incf (wire-ibuf-offset wire) length)
	       (return nil))
	      ((zerop avail)
	       (fill-input-buffer wire))
	      (t
	       (replace result
			ibuf
			:start1 offset
			:start2 (wire-ibuf-offset wire)
			:end2 (wire-ibuf-end wire))
	       (incf offset avail)
	       (decf length avail)
	       (incf (wire-ibuf-offset wire) avail)))))
    result))

;;; WIRE-GET-OBJECT -- public
;;;
;;; First, read a byte to determine the type of the object to read. Then,
;;; depending on the type, call WIRE-GET-NUMBER, WIRE-GET-STRING, or
;;; whatever to read the necessary data. Note, funcall objects are
;;; funcalled.
;;;
(defun wire-get-object (wire)
  "Read the results of `wire-output-object' and return that object."
  (let ((identifier (wire-get-byte wire))
	(*current-wire* wire))
    (declare (fixnum identifier))
    (cond ((eql identifier lookup-op)
	   (let ((index (wire-get-number wire))
		 (cache (wire-object-cache wire)))
	     (declare (integer index))
	     (declare (simple-vector cache))
	     (when (< index (length cache))
	       (svref cache index))))
	  ((eql identifier number-op)
	   (wire-get-number wire))
	  ((eql identifier bignum-op)
	   (wire-get-bignum wire))
	  ((eql identifier string-op)
	   (wire-get-string wire))
	  ((eql identifier symbol-op)
	   (let* ((symbol-name (wire-get-string wire))
		  (package-name (wire-get-string wire))
		  (package (find-package package-name)))
	     (unless package
	       (error "Attempt to read symbol, ~A, of wire into non-existent ~
		       package, ~A."
		      symbol-name package-name))
	     (intern symbol-name package)))
	  ((eql identifier cons-op)
	   (cons (wire-get-object wire)
		 (wire-get-object wire)))
	  ((eql identifier remote-op)
	   (let ((host (wire-get-number wire nil))
		 (pid (wire-get-number wire))
		 (id (wire-get-number wire)))
	     (%make-remote-object host pid id)))
	  ((eql identifier save-op)
	   (let ((index (wire-get-number wire))
		 (cache (wire-object-cache wire)))
	     (declare (integer index))
	     (declare (simple-vector cache))
	     (when (>= index (length cache))
	       (do ((newsize (* (length cache) 2)
			     (* newsize 2)))
		   ((< index newsize)
		    (let ((newcache (make-array newsize)))
		      (declare (simple-vector newcache))
		      (replace newcache cache)
		      (setf cache newcache)
		      (setf (wire-object-cache wire) cache)))))
	     (setf (svref cache index)
		   (wire-get-object wire))))
	  ((eql identifier funcall0-op)
	   (funcall (wire-get-object wire)))
	  ((eql identifier funcall1-op)
	   (funcall (wire-get-object wire)
		    (wire-get-object wire)))
	  ((eql identifier funcall2-op)
	   (funcall (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)))
	  ((eql identifier funcall3-op)
	   (funcall (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)))
	  ((eql identifier funcall4-op)
	   (funcall (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)))
	  ((eql identifier funcall5-op)
	   (funcall (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)
		    (wire-get-object wire)))
	  ((eql identifier funcall-op)
	   (let ((arg-count (wire-get-byte wire))
		 (function (wire-get-object wire))
		 (args '())
		 (last-cons nil)
		 (this-cons nil))
	     (loop
	       (when (zerop arg-count)
		 (return nil))
	       (setf this-cons (cons (wire-get-object wire)
				     nil))
	       (if (null last-cons)
		 (setf args this-cons)
		 (setf (cdr last-cons) this-cons))
	       (setf last-cons this-cons)
	       (decf arg-count))
	     (apply function args))))))


;;;; Wire output routines.

;;; WRITE-STUFF -- internal
;;;
;;; Slightly better interface to unix:unix-write. Choaks on errors.
;;;
(defmacro write-stuff (fd string-form &optional end)
  (let ((string (gensym))
	(length (gensym))
	(result (gensym))
	(error (gensym)))
    `(let* ((,string ,string-form)
	    ,@(unless end
		`((,length (length ,string)))))
       (multiple-value-bind
	   (,result ,error)
	   (unix:unix-write ,fd ,string 0 ,(or end length))
	 (cond ((null ,result)
		(error 'wire-io-error
		       :wire wire
		       :when "writing"
		       :msg (unix:get-unix-error-msg ,error)))
	       ((eql ,result ,(or end length))
		)
	       (t
		(error 'wire-io-error
		       :wire wire
		       :when "writing"
		       :msg "Not everything wrote.")))))))

;;; WIRE-FORCE-OUTPUT -- internal
;;;
;;; Output any stuff remaining in the output buffer.
;;;
(defun wire-force-output (wire)
  "Flush all internal buffers associated with $wire, sending the remote
   requests.

   This is necessary after a call to `remote'.  It is safe to call this
   when the output buffer is empty."
  (unless (zerop (wire-obuf-end wire))
    (write-stuff (wire-fd wire)
		 (wire-obuf wire)
		 (wire-obuf-end wire))
    (setf (wire-obuf-end wire) 0))
  (values))

;;; WIRE-OUTPUT-BYTE -- public
;;;
;;; Stick the byte in the output buffer. If there is no space, flush the
;;; buffer using WIRE-FORCE-OUTPUT.
;;;
(defun wire-output-byte (wire byte)
  "Output the (8-bit) byte $byte on the $wire."
  (declare (integer byte))
  (let ((fill-pointer (wire-obuf-end wire))
	(obuf (wire-obuf wire)))
    (when (>= fill-pointer (length obuf))
      (wire-force-output wire)
      (setf fill-pointer 0))
    (setf (schar obuf fill-pointer)
	  (code-char byte))
    (setf (wire-obuf-end wire) (1+ fill-pointer)))
  (values))

;;; WIRE-OUTPUT-NUMBER -- public
;;;
;;; Output the number. Note, we don't care if the number is signed or not,
;;; because we just crank out the low 32 bits.
;;;
(defun wire-output-number (wire number)
  "Output the (32-bit) number $number on $wire."
  (declare (integer number))
  (wire-output-byte wire (+ 0 (ldb (byte 8 24) number)))
  (wire-output-byte wire (ldb (byte 8 16) number))
  (wire-output-byte wire (ldb (byte 8 8) number))
  (wire-output-byte wire (ldb (byte 8 0) number))
  (values))

;;; WIRE-OUTPUT-BIGNUM -- public
;;;
;;; Output an arbitrary integer.
;;;
(defun wire-output-bignum (wire number)
  "Outputs an arbitrary integer, but less effeciently than WIRE-OUTPUT-NUMBER."
  (do ((digits 0 (1+ digits))
       (remaining (abs number) (ash remaining -32))
       (words nil (cons (ldb (byte 32 0) remaining) words)))
      ((zerop remaining)
       (wire-output-number wire
			   (if (minusp number)
			       (- digits)
			       digits))
       (dolist (word words)
	 (wire-output-number wire word)))))

;;; WIRE-OUTPUT-STRING -- public
;;;
;;; Output the string. Strings are represented by the length as a number,
;;; followed by the bytes of the string.
;;;
(defun wire-output-string (wire string)
  "Output $string on $wire.  First output the length using
   `wire-output-number', then output the bytes."
  (declare (simple-string string))
  (let ((length (length string)))
    (declare (integer length))
    (wire-output-number wire length)
    (let* ((obuf (wire-obuf wire))
	   (obuf-end (wire-obuf-end wire))
	   (available (- (length obuf)
			 obuf-end)))
      (declare (simple-string obuf)
	       (integer available))
      (cond ((>= available length)
	     (replace obuf string
		      :start1 obuf-end)
	     (incf (wire-obuf-end wire) length))
	    ((> length (length obuf))
	     (wire-force-output wire)
	     (write-stuff (wire-fd wire)
			  string))
	    (t
	     (wire-force-output wire)
	     (replace obuf string)
	     (setf (wire-obuf-end wire) length)))))
  (values))

;;; WIRE-OUTPUT-OBJECT -- public
;;;
;;; Output the given object. If the optional argument is true, cache the
;;; object to enhance the performance of sending it multiple times.
;;; Caching defaults to yes for symbols, and nil for everything else.
;;;
(defun wire-output-object (wire object &optional (cache-it (symbolp object)))
  "Send OBJECT over WIRE preceded by a tag indicating its type.

   If CACHE-IT is true, only send OBJECT the first time it gets OBJECT.
   Each end of the wire associates a token with OBJECT, similar to
   remote-objects, making the sending of object more efficient on
   successive transmissions.  CACHE-IT falls back to t for symbols and ()
   for other types.  Since the RPC level requires function names, a
   high-level protocol based on a set of function calls saves time in
   sending the functions' names repeatedly."
  (let ((cache-index (gethash object (wire-object-hash wire))))
    (cond
     (cache-index
      (wire-output-byte wire lookup-op)
      (wire-output-number wire cache-index))
     (t
      (when cache-it
	(wire-output-byte wire save-op)
	(let ((index (wire-cache-index wire)))
	  (wire-output-number wire index)
	  (setf (gethash object (wire-object-hash wire))
		index)
	  (setf (wire-cache-index wire) (1+ index))))
      (typecase object
	(integer
	 (cond ((typep object '(signed-byte 32))
		(wire-output-byte wire number-op)
		(wire-output-number wire object))
	       (t
		(wire-output-byte wire bignum-op)
		(wire-output-bignum wire object))))
	(simple-string
	 (wire-output-byte wire string-op)
	 (wire-output-string wire object))
	(symbol
	 (wire-output-byte wire symbol-op)
	 (wire-output-string wire (symbol-name object))
	 (wire-output-string wire (package-name (symbol-package object))))
	(cons
	 (wire-output-byte wire cons-op)
	 (wire-output-object wire (car object))
	 (wire-output-object wire (cdr object)))
	(remote-object
	 (wire-output-byte wire remote-op)
	 (wire-output-number wire (remote-object-host object))
	 (wire-output-number wire (remote-object-pid object))
	 (wire-output-number wire (remote-object-id object)))
	(t
	 (error "Error: Cannot output objects of type ~s across a wire."
		(type-of object)))))))
  (values))

;;; WIRE-OUTPUT-FUNCALL -- public
;;;
;;; Send the funcall down the wire. Arguments are evaluated locally in the
;;; lexical environment of the WIRE-OUTPUT-FUNCALL.
;;;
(defmacro wire-output-funcall (wire-form function &rest args)
  "Send the function and args down the wire as a funcall."
  (let ((num-args (length args))
	(wire (gensym)))
    `(let ((,wire ,wire-form))
       ,@(if (> num-args 5)
	    `((wire-output-byte ,wire funcall-op)
	      (wire-output-byte ,wire ,num-args))
	    `((wire-output-byte ,wire ,(+ funcall0-op num-args))))
       (wire-output-object ,wire ,function)
       ,@(mapcar #'(lambda (arg)
		     `(wire-output-object ,wire ,arg))
		 args)
       (values))))
