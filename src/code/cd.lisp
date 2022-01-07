;;; Compact Disk utilities.

(in-package "CD")

(use-package "ALIEN")
(use-package "C-CALL")

(export '(make-cd-image))


;;;; Variables.

(defvar *cd-block-size* 2048)
(defvar *cd-block-offset* (ash 80 -2))

(defvar *iso-type-boot-record* 0)
(defvar *iso-type-primary-volume-descriptor* 1)
(defvar *iso-type-supplementary-descriptor* 2)
(defvar *iso-type-volume-partition-descriptor* 3)
(defvar *iso-type-terminator* 4)


;;;; Palindromes.

(def-alien-type ()
  (struct unsigned-short-palindrome
    (little unsigned-short)
    (big unsigned-short)))

(def-alien-type ()
  (struct unsigned-int-palindrome
    (little unsigned-int)
    (big unsigned-int)))

(defun big-endian-short (value)
  (logior (logand (ash value 8) #xFF00) (ash value -8)))

#|
(alien:with-alien ((tem c-call:unsigned-short :local 1))
  (big-endian-short tem))
|#

(defun big-endian (value)
  (logior (logand #xFF000000 (ash value 24))
	  (logand #xFF0000 (ash value 8))
	  (logand #xFF00 (ash value -8))
	  (ash value -24)))

#|
(alien:with-alien ((tem c-call:unsigned-int :local #b01010101000000001111111100110011))
  (big-endian tem))
|#

(defun setf-short-palindrome (palindrome value)
  (setf (slot palindrome 'little) value)
  (setf (slot palindrome 'big) (big-endian-short value)))

(defun setf-palindrome (palindrome value)
  (setf (slot palindrome 'little) value)
  (setf (slot palindrome 'big) (big-endian value)))


;;;; Structure.

(def-alien-type ()
  (struct iso-date
    (year unsigned-char)
    (mon unsigned-char)
    (day unsigned-char)
    (hour unsigned-char)
    (min unsigned-char)
    (sec unsigned-char)
    (hund unsigned-char)))

(def-alien-type ()
  (struct iso-dir-entry
    (length unsigned-char)
    (ext-attr-length unsigned-char)
    (location (struct unsigned-int-palindrome))
    (size (struct unsigned-int-palindrome))
    (date (struct iso-date))
    (flags unsigned-char)
    (file-unit-size unsigned-char)
    (interleave unsigned-char)
    (volume-sequence-num (struct unsigned-short-palindrome))
    (name-len unsigned-char)
    (name unsigned-char)))

(def-alien-type ()
  (struct iso-primary-descriptor
    (type unsigned-char)
    (id (array char 5))
    (version unsigned-char)
    (unused1 unsigned-char)
    (system-id (array char 32))
    ;; 40
    (volume-id (array char 32))
    (unused2 (array unsigned-char 8))
    ;; 80
    (volume-space-size (struct unsigned-int-palindrome))
    (unused3 (array unsigned-char 32))
    ;; 120
    (volume-set-size (struct unsigned-short-palindrome))
    (volume-sequence-num (struct unsigned-short-palindrome))
    (logical-block-size (struct unsigned-short-palindrome))
    ;; 132
    (path-table-size (struct unsigned-int-palindrome))
    (type-l-path-table unsigned-int)
    (opt-type-l-path-table unsigned-int)
    (type-m-path-table unsigned-int)
    (opt-type-m-path-table unsigned-int)
    ;; 156
    (root-directory-record (struct iso-dir-entry))
    ;; 192  (+ 156 288) 444
    (volume-set-id (array char 128))
    (publisher-id (array char 128))
    (preparer-id (array char 128))
    (application-id (array char 128))
    ;;   956
    (copyright-file-id (array char 37))
    (abstract-file-id (array char 37))
    (bibliographic-file-id (array char 37))
    ;;   1067
    (creation-date (array char 17))
    (modification-date (array char 17))
    (expiration-date (array char 17))
    (effective-date (array char 17))
    ;; 883
    (file-structure-version unsigned-char)
    (unused4 unsigned-char)
    ;;   1137
    (application-data (array unsigned-char 512))
    ;;   1649
    (unused5 (array unsigned-char 653))
    ;;   2302
    ))

(def-alien-type ()
  (struct el-torito
    (w (array unsigned-short 16))
    (bootable unsigned-char) ; 88=bootable 00=not bootable
    (media-type unsigned-char) ; 0=no emulation 4=hard disk
    (load-seg unsigned-short) ; 0000->07C0
    (sys-type unsigned-char)
    (zero unsigned-char)
    (sector-cnt unsigned-short)
    (load-rba unsigned-int) ; start address of virtual disk
    (zero2 (array unsigned-char 20))))


;;;; Block writing.

;; Extend the file of $stream to be at least $end bytes long.
;;
(defun extend-file (stream end &optional (verbose t))
  (if verbose (format t "Extending file to ~A.~%" end))
  (file-position stream :end)
  (dotimes (i (- end (file-size stream)))
    (write-char (code-char 0) stream)))

;; FIX handle the length of the alien being less than *cd-block-size*
;;
(defun write-block (stream alien block-number alien-size)
  "Write $alien to $block-number of file $stream."
  (let ((end (* (1+ block-number) *cd-block-size*)))
    (if (> end (file-size stream))
	(extend-file stream end)))
  (file-position stream (* block-number *cd-block-size*))
  (format t "file pos: ~A~%" (* block-number *cd-block-size*))
  (with-alien ((buffer (* unsigned-char)
		       :local
		       (sap-alien (alien-sap alien)
				  (* unsigned-char))))
    (format t "write size: ~A~%" (min alien-size *cd-block-size*))
    (format t "write block: ~A~%" block-number)
    (format t "write alien: ~A~%" (alien-sap alien))
    (dotimes (i (min alien-size *cd-block-size*))
      (write-char (code-char (deref buffer i)) stream)))
  (force-output stream))

#|
(alien:with-alien ((tem c-call:unsigned-int :local #b01010101000000001111111100110011))
  (alien-size (struct el-torito))
  (alien-size (struct iso-primary-descriptor)))
|#


;;;; Function.

(defun string-copy (alien-array string
				&key
				(start1 0)
				(start2 0)
				(end2 (1- (length string))))
  "Copy $string into $alien-array, which must be large enough to fit
   $string."
  (when (and (plusp end2)
	     (< start2 end2))
    (format t "(string-copy ~A ~A)~%" alien-array string)
    (while ((index1 start1 (1+ index1))
	    (index2 start2 (1+ index2)))
	   ((<= index2 end2)
	    ;(setf (deref alien-array (+ start1 (- end2 start2) 1)) 0)
	    )
      (format t "i1 ~A i2 ~A end ~A~%" index1 index2 end2)
      (format t "code ~A~%" (char-code (char string index2)))
      (format t "char ~A~%" (char string index2))
      (setf (deref alien-array index1)
	    (char-code (char string index2))))))

(defun clear-alien (alien size)
  (dotimes (index size)
    (setf (deref (sap-alien (alien-sap alien)
			    (* unsigned-char)) index)
	  0)))

(defun make-cd-image (dest &key boot src verbose)
  "Write an ISO CD image to $dest.  If $boot is given, write it as the boot
   code of the image.  If $src is given, write the directory $src to the
   image."
  (if src (error "FIX $src given, still need to implement ISO FS."))
  (if (probe-file dest) (error "Destination exists: ~A" dest))
  (if boot
      (or (probe-file boot) (error "Boot image must exist: ~A" boot))
      (error "FIX Only boot images are supported."))
  (let ((iso (make-alien (struct iso-primary-descriptor)))
	(iso1 (make-alien (struct iso-primary-descriptor)))
	(iso2 (make-alien (struct iso-primary-descriptor)))
	(iso3 (make-alien (struct iso-primary-descriptor))))
    (let ((iso-primary-size (alien-size
			     (struct iso-primary-descriptor))))
      ;; FIX try set something first
      (setf (slot iso 'type) 0)
      (clear-alien iso iso-primary-size)
      (setf (slot iso1 'type) 0)
      (clear-alien iso1 iso-primary-size)
      (setf (slot iso2 'type) 0)
      (clear-alien iso2 iso-primary-size)
      (setf (slot iso3 'type) 0)
      (clear-alien iso3 iso-primary-size))
    (let ((max-depth)
	  (table-size)
	  (table-size-2)
	  (current-block))

      ;;
      ;; Collect the files in $src.
      ;; FIX

      ;;
      ;; Recurse through files, collecting size info (files, path tables).
      ;; FIX

      ;;
      ;; Set the size info.
      ;; FIX
      (setq max-depth 0)
      (setf-palindrome (slot iso 'path-table-size) 0)
      (setf-palindrome (slot iso2 'path-table-size) 0)
      (setq table-size 0)
      (setq table-size-2 0)

      (to-file (out dest)

	;;
	;; Clear the image offset.
	(if verbose (format t "Clear the image offset...~%"))
	(dotimes (i (* *cd-block-offset* *cd-block-size*))
	  (write-char (code-char 0) out))

	;;
	;; Setup more of the primary volume descriptor.
	(setf (slot iso 'type) *iso-type-primary-volume-descriptor*)
	(string-copy (slot iso 'id) "CD001")
	(setf (slot iso 'version) 1)
	(if verbose (format t "set pal~%"))
	(setf-short-palindrome (slot iso 'volume-set-size) 1)
	(setf-short-palindrome (slot iso 'volume-sequence-num) 1)
	(setf-short-palindrome (slot iso 'logical-block-size)
			       *cd-block-size*)
	(if verbose (format t "set pal done~%"))
	(setf (slot iso 'file-structure-version) 1)

	;;
	;; Setup the boot descriptor.
	(setf (slot iso1 'type) *iso-type-boot-record*)
	(string-copy (slot iso1 'id) "CD001")
	(setf (slot iso1 'version) 1)
	;; FIX LoseThos starts this in the byte before this slot.
	;(string-copy (slot iso1 'system-id) "EL TORITO SPECIFICATION")
	(string-copy (sap-alien (alien-sap (addr (slot iso1 'unused1)))
				(array unsigned-char 33))
		     "EL TORITO SPECIFICATION")

	;;
	;; Setup more of the supplementary descriptor.
	(setf (slot iso2 'type) *iso-type-supplementary-descriptor*)
	(if verbose (format t "copy~%"))
	(string-copy (slot iso2 'id) "CD001")
	(if verbose (format t "copy done~%"))
	(setf (slot iso2 'version) 1)
	(if verbose (format t "set pal~%"))
	(setf-short-palindrome (slot iso2 'volume-set-size) 1)
	(setf-short-palindrome (slot iso2 'volume-sequence-num) 1)
	(setf-short-palindrome (slot iso2 'logical-block-size) *cd-block-size*)
	(if verbose (format t "set pal done~%"))
	(setf (slot iso2 'file-structure-version) 1)

	;;
	;; Setup the terminating descriptor.
	(setf (slot iso3 'type) *iso-type-terminator*)
	(string-copy (slot iso3 'id) "CD001")
	(setf (slot iso3 'version) 1)
	(format t "iso3: ~A~%" (alien-sap iso3))
	(format t "iso3 type: ~A~%" (slot iso3 'type))
	(format t "iso3 version: ~A~%" (slot iso3 'version))

	(setq current-block *cd-block-offset*)

	;;
	;; Write any boot code.
	(when boot
	  (if verbose (format t "Write boot code...~%"))
	  (with-alien ((el-torito (struct el-torito)))
	    (clear-alien el-torito (alien-size (struct el-torito)))

	    (setf (deref (slot iso1 'volume-id) 31) current-block)

	    (setf (deref (slot el-torito 'w) 0) 1)

	    (string-copy (cast (slot el-torito 'w)
			       (array unsigned-char 32))
			 "LoseThos" ; FIX "Nightshade"
			 :start1 4)

	    (setf (deref (slot el-torito 'w) 15) #xAA55)

#|
	    (with-alien ((checksum unsigned-char :local 0))
	      (dotimes (index 16)
		(incf checksum
		      (deref (slot el-torito 'w) index)))
	      (setf (deref (slot el-torito 'w) 14) (- checksum)))
|#

	    (let ((checksum 0)) ; FIX losethos uses a U8 (8 bytes)
	      (dotimes (index 16)
		(incf checksum
		      (deref (slot el-torito 'w) index)))
	      (if verbose (format t "checksum ~A~%" checksum))
	      (setf (deref (cast (slot el-torito 'w)
				 (* short))
			   14)
		    (- (logand #xFFFF checksum))))

	    (setf (slot el-torito 'bootable) #x88
		  ; "0=no emu 2=1.44meg 4=hard drive"
		  (slot el-torito 'media-type) 0
		  (slot el-torito 'sector-cnt) 4
		  (slot el-torito 'load-rba) (1+ current-block))

	    (if verbose (format t "~A: Pre boot block.~%" current-block))
	    (write-block out
			 el-torito
			 current-block
			 (alien-size (struct el-torito)))
	    (incf current-block)

	    (if verbose (format t "~A: Boot block.~%" current-block))
	    (let* ((buffer (from-file (in boot)
			    (with-output-to-string (out)
			      (transfer in out))))
		   (length (length buffer)))
	      (cond ((< length *cd-block-size*)
		     #| The end of the block should be clear already.
		     (dotimes (i (- *cd-block-size* length))
		       (write-char (code-char (deref buffer i)) stream))
		     |#)
		    ((> length *cd-block-size*)
		     (warn "Boot buffer bigger than CD block, truncating.")
		     ;; Cut off the end of the buffer.
		     (setq buffer (subseq buffer 0 *cd-block-size*))))
	      (if verbose (format t "Boot buffer: ~A~%" buffer))
	      (let ((end (* (1+ current-block) *cd-block-size*)))
		(if (> end (file-size out))
		    (extend-file out end)))
	      (with-input-from-string (in buffer)
		(file-position out (* current-block *cd-block-size*))
		(transfer in out)))
	    (incf current-block)))

	;;
	;; Write the files (as an ISO file system).

	;; FIX

	;;
	;; Write the big and little endian path tables and fill in the
	;; remaining descriptor info.

	;; FIX

	(if verbose (format t "Volume space size: ~A~%" current-block))
	(setf-palindrome (slot iso 'volume-space-size) current-block)
	(setf-palindrome (slot iso2 'volume-space-size) current-block)

	;;
	;; Write the descriptors.
	(let ((iso-primary-size (alien-size
				 (struct iso-primary-descriptor))))
	  (if verbose (format t "Write primary descriptor...~%"))
	  (write-block out iso 16 iso-primary-size)
	  (if boot
	      (progn
		(if verbose (format t "Write boot descriptor...~%"))
		(write-block out iso1 17 iso-primary-size)
		(if verbose (format t "Write supplementary descriptor...~%"))
		(write-block out iso2 18 iso-primary-size)
		(if verbose (format t "Write terminating descriptor...~%"))
		(write-block out iso3 19 iso-primary-size))
	      (progn
		(if verbose (format t "Write supplementary descriptor...~%"))
		(write-block out iso2 17 iso-primary-size)
		(if verbose (format t "Write terminating descriptor...~%"))
		(write-block out iso3 18 iso-primary-size)))
	  (if verbose (format t "Done.~%")))))
    ;; FIX unwind-protect
    #|
    ;; FIX double free or corruption
    (free-alien iso)
    (free-alien iso1)
    (free-alien iso2)
    (free-alien iso3)
    |#
    ))

#|
(alien:with-alien ((tem (struct iso-primary-descriptor)))
  (values (addr tem) (addr (slot tem 'file-structure-version))))

(make-cd-image ":tmp/cd.iso" :verbose t :boot ":tmp/boot.core" :src ":tmp/fs/")

(make-cd-image ":tmp/cd.iso" :verbose t :boot ":tmp/missing.core")

(make-cd-image ":tmp/exists.iso" :verbose t :boot ":tmp/boot.core")

(delete-file ":tmp/cd.iso")
(make-cd-image ":tmp/cd.iso" :verbose t :boot ":tmp/boot.core")
|#
