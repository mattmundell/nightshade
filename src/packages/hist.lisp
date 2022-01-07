;;; -*- Package: HIST -*-
;;;
;;; Simple textual Histogram facility.
;;;
;;; Wish list: Some sort of automatic scaling for the number and size of
;;; buckets would be nice, if the user chooses not to supply these.  This
;;; would probably require running the body twice, once to determine the
;;; spread of values, and again to actually produce the histogram.

(defpackage "HIST"
  (:version 0)
  (:use "LISP")
  (:export "HIST" "HIST-RECORD")
  (:documentation
   "Simple textual histogram facility.

Hist is a macro of form (HIST (min max [bucket-size]) . body)

Creates a histogram with buckets of the specified size (defaults to 1),
spanning the range from Low (inclusive) to High (exclusive), with two
additional buckets to catch values below and above this range.  The body is
executed as a progn, and every call to Hist-Record within the body provides a
value for the histogram to count.  When Body exits, the histogram is printed
out and Hist returns Nil.

A simple example:
   (hist (0 10) (dotimes (i 1000) (random 10)))
This example may make the RANDOM distribution look more normal:
   (hist (0 10 2) (dotimes (i 1000) (random 10)))
This example will show you overflow buckets:
   (hist (2 12) (dotimes (i 1000) (random 15)))"))

(in-package "HIST")
(export '(hist hist-record))

;;; Uses a bunch of specials for communication.

(proclaim '(special *hist-lower-limit*
		    *hist-upper-limit*
		    *hist-bucket-size*
		    *hist-nbuckets*
		    *hist-array*))

(defconstant hist-limit 60
  "The maximum number of #'s that are to be printed.")

(defmacro hist ((low high &optional (bucket-size 1))
		&body body)
  "Format is (HIST (low high [bucket-size]) . body).

   Creates a histogram with buckets of the specified size (defaults to 1),
   spanning the range from Low (inclusive) to High (exclusive), with two
   additional buckets to catch values below and above this range.
   The body is executed as a progn, and every call to Hist-Record within
   the body provides a value for the histogram to count.
   When Body exits, the histogram is printed out and Hist returns Nil."
  `(let* ((*hist-lower-limit* ,low)
	  (*hist-upper-limit* ,high)
	  (*hist-bucket-size* ,bucket-size)
	  (*hist-nbuckets*
	   (+ 2 (ceiling (- *hist-upper-limit* *hist-lower-limit*)
			 *hist-bucket-size*)))
	  (*hist-array* (make-array *hist-nbuckets* :initial-element 0)))
     (progn ,@body)
     (let ((biggest 0) (scale 1))
       (dotimes (b (- *hist-nbuckets* 2))
	 (when (> (svref *hist-array* b) biggest)
	   (setq biggest (svref *hist-array* b))))
       (when (> biggest hist-limit)
	 (setq scale (ceiling biggest hist-limit))
	 (format t "~&Each \"#\" equals ~S units.  The \".\" indicates a fraction."
		 scale))
       (format t "~&< ~S: ~12,8T~S~%"
	       *hist-lower-limit*
	       (svref *hist-array* (1- *hist-nbuckets*)))
       (do ((b 0 (1+ b))
	    (bval *hist-lower-limit* (+ bval *hist-bucket-size*))
	    (bcount 0))
	   ((= b (- *hist-nbuckets* 2)))
	 (setq bcount (svref *hist-array* b))
	 (multiple-value-bind (q r) (truncate bcount scale)
	   (format t "~S: ~12,8T~S~20,8T~V,1,0,'#@A~%"
		   bval bcount (1+ q) (if (zerop r) #\  #\.))))
       (format t "> ~S: ~12,8T~S~%"
	       *hist-upper-limit*
	       (svref *hist-array* (- *hist-nbuckets* 2))))
     nil))

(defun hist-record (value)
  "This function should only be called within the body of a HIST form.
  Increments the proper histogram counter to record this value."
  (cond ((< value *hist-lower-limit*)
	 (incf (svref *hist-array* (1- *hist-nbuckets*))))
	((>= value *hist-upper-limit*)
	 (incf (svref *hist-array* (- *hist-nbuckets* 2))))
	(t (incf (svref *hist-array* (floor (- value *hist-lower-limit*)
					    *hist-bucket-size*))))))
