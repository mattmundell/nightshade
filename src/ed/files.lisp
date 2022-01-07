;;; File manipulation functions.

(in-package "EDI")

(export '(read-file write-file))


#[ Files

These commands either read a file into the current buffer or write it out to
some file.  Various other bookkeeping operations are performed as well.

{command:Find File}
{command:Save File}
{command:Save All Files}
{command:Save All Files and Exit}
{evariable:Save All Files Confirm}
{command:Visit File}
{command:Write File}
{command:Backup File}
{command:Revert File}
{evariable:Revert File Confirm}
{command:Insert File}
{command:Write Region}
{evariable:Add End Newline on Writing File}
{evariable:Keep Backup Files}

[ Auto Save Mode                  ]
[ Filename Defaulting and Merging ]
[ Type Hooks and File Options     ]
]#

#[ Files (extension)

This chapter discusses ways to read and write files at various levels -- at
marks, into regions, and into buffers.  This also treats automatic mechanisms
that affect the state of buffers in which files are read.

[ File Options and Type Hooks ]
[ Pathnames and Buffers       ]
[ File Groups (extension)     ]
[ File Reading and Writing    ]
]#


;;;; Utility functions.

(defun find-char-from-sap (sap start end char)
  (declare (type system-area-pointer sap)
	   (type (integer 0 (#.most-positive-fixnum)) start end)
	   (type base-char char))
  (do ((index start (1+ index))
       (code (char-code char)))
      ((>= index end) nil)
    (declare (type (integer 0 #.most-positive-fixnum) index)
	     (type (unsigned-byte 8) code))
    (when (= (sap-ref-8 sap index) code)
      (return index))))


#[ File Reading and Writing

Lisp pathnames are used by the file primitives.  For probing, checking
write dates, and so forth, all of the Lisp file functions are available.

{function:ed:read-file}
{function:ed:write-file}
{function:ed:write-buffer-file}
{evariable:Add End Newline on Writing File}
{evariable:Write File Hook}
{function:ed:read-buffer-file}
{evariable:Read File Hook}
{function:ed:find-file-buffer}
]#


;;;; File type pre-read conversion handling.

;;; FIX this could easily be more easily extensible

(defun convert-for-buffer (file buffer)
  "Convert $file according to it's type, returning the resulting file.  The
   returned file is to be read into $buffer.  Set *Ed Converted Info* to
   '(type pathname) where type is a string and pathname is the returned
   file name."
  (let ((type (pathname-type file)))
    (cond ((string= type "gz")
	   (let* ((new (pick-new-file)) ;; FIX release with buffer
		  (ret (with-open-file (stream
					new
					:direction :output)
			 (ext:run-program "gunzip"
					  (list "--stdout"
						(namestring file))
					  :output stream
					  :error t))))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (1 (message "gunzip exited with error."))
		 (2 (message "gunzip exited with warning."))))
	     (if (editor-bound-p 'ed::ed-converted-info :buffer buffer)
		 (setf (variable-value 'ed::ed-converted-info :buffer buffer)
		       (list type new))
		 (defevar "Ed Converted Info"
		   "Conversion info for current file.  Set in `read-file'."
		   :value (list type new)
		   :buffer buffer))
	     new))
#|
	  ((string= type "bz2")
	   (let ((ret (ext:run-program "bunzip2" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (t (message "bunzip2 exited with error."))))
	     (subseq file 0 (- (length file) (length type) 1))))
	  ((string= type "zip")
	   (let ((ret (ext:run-program "unzip" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (t (message "unzip exited with error."))))
	     (subseq file 0 (- (length file) (length type) 1))))
	  ((string= type "Z")
	   (let ((ret (ext:run-program "uncompress" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (t (message "uncompress exited with error."))))
	     (subseq file 0 (- (length file) (length type) 1))))
|#
	  (t
	   (if (editor-bound-p 'ed::ed-converted-info :buffer buffer)
	       (setf (variable-value 'ed::ed-converted-info :buffer buffer)
		     ()))
	   file))))

(defun convert-for-file (type converted-pathname pathname)
  "Convert $converted-pathname according to it's type, writing the output
   to $pathname."
  (ecase= type
    ("gz"
     (ext:run-program "gzip" (list converted-pathname))
     (rename-file (concat converted-pathname ".gz") pathname))
#|
	  ((string= type "bz2")
	   (let ((ret (ext:run-program "bunzip2" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (t (message "bunzip2 exited with error."))))
	     (subseq file 0 (- (length file) (length type) 1))))
	  ((string= type "zip")
	   (let ((ret (ext:run-program "unzip" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (t (message "unzip exited with error."))))
	     (subseq file 0 (- (length file) (length type) 1))))
	  ((string= type "Z")
	   (let ((ret (ext:run-program "uncompress" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (t (message "uncompress exited with error."))))
	     (subseq file 0 (- (length file) (length type) 1))))
|#
    ))


;;;; Read-File.

;; FIX consider this as a general stream op
; (defun read-file (pathname stream)
;   "Write the contents of the file named by PATHNAME into STREAM."
; (defun transfer (in out)
;   "Read to the end of stream IN, writing the content to stream OUT."
;; FIX code:stream.lisp:transfer
(defun read-file (pathname mark)
  "Insert the contents of the file named by $pathname at $mark."
  (with-mark ((mark mark :left-inserting))
    (if (remote-pathname-p pathname)
	;; FIX equiv convert-for-buffer
	(with-output-to-mark (stream mark)
	  (internet:read-remote-file pathname stream))
	(let* ((tn (truename pathname))
	       (name (convert-for-buffer (namestring tn)
					 (mark-buffer mark)))
	       (sap nil)
	       (size 0))
	  (declare (fixnum size)
		   (type (or null system-area-pointer) sap))
	  (multiple-value-bind (fd err) (unix:unix-open name unix:o_rdonly 0)
	    (when fd
	      (multiple-value-bind (res dev ino mode nlnk uid gid rdev len)
				   (unix:unix-fstat fd)
		(declare (ignore ino mode nlnk uid gid rdev))
		(cond ((null res)
		       (setq err dev))
		      (t
		       ;; When len is 0 try read into a buffer of 4096, in
		       ;; case this is a Linux /proc file.  The number 4096
		       ;; is fairly arbitary, in some circumstance it is
		       ;; the the memory page size and hence the maximum
		       ;; size of a proc file.  Fail if this buffer is
		       ;; filled.  FIX Retry with a bigger buffer instead.
		       (setf sap
			     (system:allocate-system-memory
			      (if (zerop len) 4096 len)))
		       (setf size len)
		       (multiple-value-bind
			   (bytes err3)
			   (unix:unix-read fd sap (if (zerop len) 4096 len))
			 (if (null bytes)
			     (setq err err3)
			     (if (zerop len)
				 (if (= bytes 4096)
				     (error "Reading \"0\" sized file filled buffer.")
				     (setf size bytes
					   err ()))
				 (if (< bytes len)
				     (setq err err3)
				     (setq err ()))))))))
	      (unix:unix-close fd))
	    (if err
		(error "Reading file ~A, unix error ~A."
		       name (unix:get-unix-error-msg err)))
	    (if (zerop size) (return-from read-file nil))
	    (let* ((first-line (mark-line mark))
		   (buffer (line-%buffer first-line))
		   (index (find-char-from-sap sap 0 size #\newline)))
	      (declare (type (or null (integer 0 (#.most-positive-fixnum)))
			     index))
	      (modifying-buffer buffer)
	      (let* ((len (or index size))
		     (chars (make-string len)))
		(%primitive byte-blt sap 0 chars 0 len)
		(insert-string mark chars))
	      (when index
		(insert-character mark #\newline)
		(do* ((old-index (1+ index) (1+ index))
		      (index (find-char-from-sap sap old-index size #\newline)
			     (find-char-from-sap sap old-index size #\newline))
		      (number (+ (line-number first-line) line-increment)
			      (+ number line-increment))
		      (previous first-line))
		     ((not index)
		      (let* ((length (- size old-index))
			     (chars (make-string length))
			     (line (mark-line mark)))
			(declare (fixnum length))
			(%primitive byte-blt sap old-index chars 0 length)
			(insert-string mark chars)
			(setf (line-next previous) line)
			(setf (line-previous line) previous)
			(do ((line line (line-next line))
			     (number number (+ number line-increment)))
			    ((null line))
			  (declare (fixnum number))
			  (setf (line-number line) number))))
		  (declare (fixnum number old-index))
		  (let ((line (make-line
			       :previous previous
			       :%buffer buffer
			       :number number
			       :chars (system:sap+ sap old-index)
			       :buffered-p
			       (the fixnum (- (the fixnum index) old-index)))))
		    (setf (line-next previous) line)
		    (setq previous line))) nil)))))))

;;; Hackish stuff for speed.

(defun read-buffered-line (line)
  (let* ((len (line-buffered-p line))
  	 (chars (make-string len)))
    (%primitive byte-blt (line-%chars line) 0 chars 0 len)
    (setf (line-buffered-p line) nil)
    (setf (line-chars line) chars)))


;;;; Write-File.

(defun write-file (region pathname &key append
			  (keep-backup (value ed::keep-backup-files))
			  access)
  "Write the contents of $region to the file named by $pathname.  Write
   region using a stream as if it were `open'ed with :if-exists supplied as
   :rename-and-delete.

   When $keep-backup, which falls back to the value of *Keep Backup Files*,
   is true, `open' the stream as if :if-exists were :rename.  If $append is
   true, write the file as if it were `open'ed with :if-exists supplied as
   :append.

   Signal an error if both $append and $keep-backup are true.

   $access is an implementation dependent value that is suitable for
   setting pathname's access or protection bits."
  (let ((if-exists-action (cond ((and keep-backup append)
				 (error "True values for supplied for ~
				         both keep-backup and append."))
				(keep-backup :rename)
				(append :append)
				(t :rename-and-delete))))
    (if (remote-pathname-p pathname)
	;; FIX equiv convert-for-file...
	(with-input-from-region (in region)
	  (internet:write-remote-file pathname in if-exists-action))
	(let* ((buffer (mark-buffer (region-start region)))
	       (convert-info (if (editor-bound-p 'ed::ed-converted-info
						 :buffer buffer)
				 (variable-value 'ed::ed-converted-info
						 :buffer buffer))))
	  (with-open-file (file (or (cadr convert-info) pathname)
				:direction :output
				:element-type 'base-char
				:if-exists if-exists-action)
	    (close-line)
	    (fast-write-file region file))
	  (if convert-info (convert-for-file (car convert-info)
					     (cadr convert-info)
					     pathname))
	  (when access
	    (multiple-value-bind
		(winp code)
		;; Must do a TRUENAME in case the file has never been written.
		;; It may have Lisp syntax that Unix can't handle.
		;; If this is ever moved to the beginning of this function to use
		;; Unix CREAT to create the file protected initially, then TRUENAME
		;; will signal an error, and LISP::PREDICT-NAME will have to be used.
		;; FIX where predict-name?
		(unix:unix-chmod (namestring (truename pathname)) access)
	      (fi winp
		  (error "Failed to set access code: ~S"
			 (unix:get-unix-error-msg code)))))))))

#|
(defun fast-write-file (region file)
  (let* ((start (region-start region))
	 (start-line (mark-line start))
	 (start-charpos (mark-charpos start))
	 (end (region-end region))
	 (end-line (mark-line end))
	 (end-charpos (mark-charpos end)))
    (if (eq start-line end-line)
	(write-string (line-chars start-line) file
		      :start start-charpos :end end-charpos)
	(let* ((first-length (- (line-length start-line) start-charpos))
	       (length (+ first-length end-charpos 1)))
	  (do ((line (line-next start-line) (line-next line)))
	      ((eq line end-line))
	    (incf length (1+ (line-length line))))
	  (let ((sap (system:allocate-system-memory length)))
	    (unwind-protect
		(macrolet ((chars (line)
			     `(if (line-buffered-p ,line)
				  (line-%chars ,line)
				  (line-chars ,line))))
		  (system:%primitive byte-blt
				     (chars start-line) start-charpos
				     sap 0 first-length)
		  (setf (system:sap-ref-8 sap first-length)
			(char-code #\newline))
		  (let ((offset (1+ first-length)))
		    (do ((line (line-next start-line)
			       (line-next line)))
			((eq line end-line))
		      (let ((end (+ offset (line-length line))))
			(system:%primitive byte-blt
					   (chars line) 0
					   sap offset end)
			(setf (system:sap-ref-8 sap end)
			      (char-code #\newline))
			(setf offset (1+ end))))
		    (or (zerop end-charpos)
			(system:%primitive byte-blt
					   (chars end-line) 0
					   sap offset
					   (+ offset end-charpos))))
		  (multiple-value-bind
		      (okay errno)
		      (unix:unix-write (system:fd-stream-fd file)
				       sap 0 length)
		    (fi okay
			(error "Failed to write ~S: ~A"
			       file
			       (unix:get-unix-error-msg errno)))))
	      (system:deallocate-system-memory sap length)))))))
|#

(defun fast-write-file (region file)
  "Write $region to $file."
  (let* ((start (region-start region))
	 (start-line (mark-line start))
	 (start-charpos (mark-charpos start))
	 (end (region-end region))
	 (end-line (mark-line end))
	 (end-charpos (mark-charpos end)))
    (if (eq start-line end-line)
	(write-string (line-chars start-line) file
		      :start start-charpos :end end-charpos)
	(let* ((first-length (- (line-length start-line) start-charpos))
	       (total-length (+ first-length end-charpos 1)))
	  ;; Count the bytes in the buffer.
	  (do ((line (line-next start-line) (line-next line)))
	      ((eq line end-line))
	    (incf total-length (1+ (line-length line))))
	  ;; Loop writing as many bytes as a SAP can hold.  For most files
	  ;; this writes all the bytes at once.
	  (do* ((sap-length)
		(left total-length (- left sap-length))
		;; The length of a SAP that `byte-blt' can write is limited
		;; to vm::max-bits.
		;; FIX ::s
		(sap-length-left (if (> left vm::max-bits)
				     vm::max-bits left)
				 (if (> left vm::max-bits)
				     vm::max-bits left)))
	       ((zerop left))
	    (setq sap-length sap-length-left)
	    (let ((sap (system:allocate-system-memory sap-length)))
	      ;; Write sap-length bytes to the file.
	      ;;
	      (unwind-protect
		  (macrolet ((chars (line)
			       `(if (line-buffered-p ,line)
				    (line-%chars ,line)
				    (line-chars ,line))))
		    (let ((offset 0))
		      ;; Loop over the lines, filling the SAP.  Take care,
		      ;; as the line may be too long for the SAP.
		      (do ((line start-line (line-next line)))
			  ((or (zerop sap-length-left) (fi line))
			   ;; Write the SAP out to the file.
			   (multiple-value-bind
			       (okay errno)
			       (unix:unix-write (system:fd-stream-fd file)
						sap 0 sap-length)
			     (or okay
				 (error "Failed to write ~S: ~A"
					file
					(unix:get-unix-error-msg errno)))))
			(let* ((write-len (if (> (- (line-length line)
						    start-charpos)
						 sap-length-left)
					      sap-length-left
					      (- (line-length line)
						 start-charpos)))
			       (end (+ offset write-len)))
			  ;; Copy from the buffer to the SAP.
			  (system:%primitive byte-blt
					     (chars line) start-charpos
					     sap offset end)
			  (decf sap-length-left write-len)
			  (setf offset end)
			  (if (zerop sap-length-left)
			      ;; The SAP is full and the newline must still
			      ;; be added to it.
			      (setf start-charpos (line-length line)
				    start-line line)
			      (progn
				;; Add the newline to the SAP.
				(setf (system:sap-ref-8 sap end)
				      (char-code #\newline))
				(setq start-charpos 0)
				(if (zerop (decf sap-length-left))
				    ;; The SAP is full.
				    (setq start-line (line-next line))
				    (incf offset))))))))
		(system:deallocate-system-memory sap sap-length))))))))
