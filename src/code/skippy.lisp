;;;; 
;;;; skippy.lisp
;;;; 
;;;; Created: 2005-03-06 by Zach Beane <xach@xach.com>
;;;; 
;;;; GIF-writing functions. Only two functions are exported. Example
;;;; usage:
;;;;
;;;; (make-instance 'gif
;;;;                :height 10 :width 10 :bpp 8
;;;;                :color-table <768-entry color table>
;;;;                :image-data <100-entry index vector>)
;;;;
;;;; (write-gif <gif instance> pathname)
;;;;
;;;;
;;;; The color table must be an (unsigned-byte 8) vector with (* 3
;;;; (expt bpp 2)) entries; the table is in the format
;;;;
;;;;    #(red0 green0 blue0 ... redN greenN blueN)
;;;;
;;;;
;;;; See also:
;;;;
;;;;   - CompuServe Incorporated, "Graphics Interchange Format, Version 89a"
;;;;
;;;;   - Steve Blackstock, "LZW and GIF explained"
;;;;
;;;; Both of the above are available from:
;;;;
;;;;   http://www.dcs.ed.ac.uk/home/mxr/gfx/2d-hi.html
;;;;
;;;;
;;;; This file is released into the public domain.
;;;;
;;;; $Id: skippy.lisp,v 1.12 2005/12/22 18:24:56 xach Exp $


(defpackage :skippy
  (:nicknames :gif)
  (:use :cl)
  (:export :gif
           :write-gif
           :write-image
           :image
           :frame
           :width
           :height
           :output-image
           :frames
           :comment
           :delay-time
           :last-frame))

(in-package :gif)

(defun write-uint16 (number stream)
  (write-byte (logand #xFF number) stream)
  (write-byte (ash number -8) stream))


;;; The GIF structure

(defclass gif ()
  ((height :initarg :height :reader height)
   (width :initarg :width :reader width)
   (bpp :initarg :bpp :reader bpp)
   (image-data :initarg :image-data :reader image-data)
   (color-table :initarg :color-table :reader color-table)))


(defmethod initialize-instance :after ((gif gif) &key &allow-other-keys)
  (assert (= (length (image-data gif)) (* (height gif) (width gif))))
  (assert (= (length (color-table gif)) (* 3 (expt 2 (bpp gif))))))



;;; Image data is output in blocks of 255 bytes. The bytes are filled
;;; in variable-length chunks.
;;;
;;; FIXME: This is a very good candidate for optimization.

(defclass image-data-stream ()
  ((bit-index :initform 0 :accessor bit-index)
   (byte-index :initform 0 :accessor byte-index)
   (buffer :initform (make-array 255
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)
           :reader buffer)
   (output-stream :initarg :output-stream :reader output-stream)))

(defgeneric reset-stream (stream))
(defgeneric write-buffer (stream))
(defgeneric write-bit (bit stream))
(defgeneric write-bits (integer length stream))

(defmethod reset-stream ((stream image-data-stream))
  (setf (bit-index stream) 0
        (byte-index stream) 0)
  (fill (buffer stream) 0))

(defmethod write-buffer ((stream image-data-stream))
  (with-slots (output-stream byte-index bit-index)
      stream
    (unless (and (zerop bit-index) (zerop byte-index))
      (unless (zerop bit-index)
        (incf byte-index))
      (write-byte byte-index output-stream)
      (write-sequence (buffer stream) output-stream :end byte-index)
      (reset-stream stream))))

(defmethod write-bit (bit (stream image-data-stream))
  (with-slots (bit-index byte-index buffer output-stream)
      stream
    (setf (aref buffer byte-index)
          (logior (ash bit bit-index) (aref buffer byte-index)))
    (incf bit-index)
    (when (= bit-index 8)
      (setf bit-index 0)
      (incf byte-index))
    (when (= byte-index 255)
      (write-buffer stream))))

(defmethod write-bits (integer length (stream image-data-stream))
  (loop for i = integer then (ash i -1)
        repeat length do
        (write-bit (logand #x01 i) stream)))


;;; Testing this crap

(defun file->octets (file)
  (with-open-file (stream file
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun test-write-bits (file)
  (with-open-file (stream file
                   :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :supersede)
    (let ((data-stream (make-instance 'image-data-stream
                                      :output-stream stream)))
      (dotimes (i 4)
        (write-bits 1 10  data-stream))
      (write-buffer data-stream)))
  (file->octets file))

(defmacro def-bit-test (name (&rest pairs) result)
  `(defun ,name ()
     (with-open-file (stream "bits.dat"
                      :direction :output
                      :element-type '(unsigned-byte 8)
                      :if-exists :supersede)
       (let ((data-stream (make-instance 'image-data-stream
                                         :output-stream stream)))
         ,@(loop for (number width) on pairs by #'cddr
                 collect `(write-bits ,number ,width data-stream))
         (write-buffer data-stream)))
     (let ((bytes (file->octets "bits.dat")))
       (when (not (equalp bytes ,result))
         (format t "FAILED: ~A /= ~A~%" bytes ,result)))))

(def-bit-test bit-test.1
    (1 8 1 8 1 8 7 7)
  #(4 1 1 1 7))

(def-bit-test bit-test.2
    (0 4 #b1111 4)
  #(1 #xF0))

(def-bit-test bit-test.3
    (#b1111 4 0 4)
  #(1 #x0F))

(def-bit-test bit-test.4
    (1 7 0 7)
  #(2 1 0))

(def-bit-test bit-test.5
    (1 3  1 3  0 3  1 3)
  #(2 #b00001001 #b00000010))
       

                  
        

;;; Converting image data to a compressed form

(defun lzw-compress (vector code-size stream)
  (let ((iv 0)
        (data-stream (make-instance 'image-data-stream :output-stream stream)))
    (flet ((next-input ()
             (when (< iv (length vector))
               (prog1
                   (aref vector iv)
                 (incf iv)))))
      (let* ((string-table (make-hash-table))
             (clear-code (expt 2 code-size))
             (end-of-input-code (1+ clear-code))
             (index (+ 2 clear-code))
             (compression-size (1+ code-size))
             (max-index (1- (expt 2 compression-size)))
             (prefix (next-input))
             (next-char nil))
        (flet ((output-code (code)
                 (write-bits code compression-size data-stream)))
          (output-code clear-code)
          (loop
           (setf next-char (next-input))
           (when (null next-char)
             (output-code prefix)
             (output-code end-of-input-code)
             (write-buffer data-stream)
             (return))
           (let* ((key (logior (ash prefix 8) next-char))
                  (entry (gethash key string-table)))
             (cond (entry
                    (setf prefix entry))
                   (t
                    (output-code prefix)
                    (setf (gethash key string-table) index)
                    (when (> index max-index)
                      (setf max-index (1- (expt 2 (incf compression-size)))))
                    (incf index)
                    (setf prefix next-char))))
           (when (= index #xFFF)
             ;; The index isn't allowed to be this big, so the string
             ;; table must be cleared out and restarted
             (output-code clear-code)
             (setf compression-size (1+ code-size))
             (setf max-index (1- (expt 2 compression-size)))
             (clrhash string-table)
             (setf index (+ 2 clear-code)))))))))



;;; Writing out the GIF file format

(defvar *gif-signature* #(71 73 70 56 57 97)
  "The string \"GIF89a\" in ASCII.")

(defconstant +global-color-table-present+ 0
  "Global color tables are not supported.")

(defconstant +pixel-aspect-ratio+ 0
  "Pixel aspect ratios are not set.")

(defconstant +image-separator-code+ #x2C)

(defconstant +gif-trailer-code+ #x3B
  "The end-of-GIF marker.")

(defconstant +image-left-position+ 0
  "Since there's only one image in the stream, put it all the way to
the left.")

(defconstant +image-top-position+ 0
  "Since there's only one image in the stream, put it all the way at
the top.")

(defconstant +local-color-table-flag+ #x80
  "The bit for the local color table is always set.")

(defun write-gif-header (gif stream)
  (write-sequence *gif-signature* stream)
  (write-uint16 (width gif) stream)
  (write-uint16 (height gif) stream)
  (write-byte (ash (1- (bpp gif)) 4) stream)
  (write-byte +global-color-table-present+ stream)
  (write-byte +pixel-aspect-ratio+ stream))

(defun write-image-descriptor (gif stream)
  (write-byte +image-separator-code+ stream)
  (write-uint16 +image-left-position+ stream)
  (write-uint16 +image-top-position+ stream)
  (write-uint16 (width gif) stream)
  (write-uint16 (height gif) stream)
  (write-byte (logior +local-color-table-flag+
                      (1- (bpp gif)))
              stream))

(defun write-local-color-table (gif stream)
  (write-sequence (color-table gif) stream))

(defun write-image-data (gif stream)
  (write-byte (bpp gif) stream)
  (lzw-compress (image-data gif) (bpp gif) stream)
  (write-byte 0 stream))

(defun write-gif (gif file &key (if-exists :supersede))
  "Write the GIF object to FILE, returning FILE when finished."
  (with-open-file (stream file
                   :direction :output
                   :if-exists if-exists
                   :element-type '(unsigned-byte 8))
    (write-gif-header gif stream)
    (write-image-descriptor gif stream)
    (write-local-color-table gif stream)
    (write-image-data gif stream)
    (write-byte +gif-trailer-code+ stream)
    file))



;;; Animation classes

(defclass image ()
  ((height
    :initarg :height
    :accessor height
    :documentation "The logical height of the image.")
   (width
    :initarg :width
    :accessor width
    :documentation "The logical width of the image.")
   (color-table
    :initarg :color-table
    :accessor color-table
    :initform nil
    :documentation "The (optional) global color table for the image.")
   (loopingp
    :initarg :loopingp
    :accessor loopingp
    :initform nil)
   (comment
    :initarg :comment
    :accessor comment
    :initform nil)
   (frames
    :initarg :frames
    :accessor frames
    :initform (make-array 10 :adjustable t :fill-pointer 0)
    :documentation "A list of the frames in the image.")))

(defclass frame ()
  ((image
    :initarg :image
    :accessor image
    :documentation "The image in which this frame occurs.")
   (data
    :initarg :data
    :accessor data)
   (height
    :initarg :height
    :accessor height)
   (width
    :initarg :width
    :accessor width)
   (offset-top
    :initarg :offset-top
    :accessor offset-top
    :initform 0
    :documentation "The offset from the top of the logical image boundary.")
   (offset-left
    :initarg :offset-left
    :accessor offset-left
    :initform 0
    :documentation "The offset from the left of the logical image boundary.")
   (color-table
    :initarg :color-table
    :accessor color-table
    :initform nil
    :documentation "The local color table of the image, if any.")
   (interlacedp
    :initarg :interlacedp
    :accessor interlacedp
    :initform nil)
   (disposal-method
    :initarg :disposal-method
    :accessor disposal-method
    :initform :unspecified)
   (delay-time
    :initarg :delay-time
    :accessor delay-time
    :initform 0
    :documentation "The time, in hundredths of a second, to wait after
this frame before displaying the next frame.")
   (transparency-index
    :initarg :transparency-index
    :accessor transparency-index
    :initform nil
    :documentation "The index of the transparent color for this
frame. If null, frame has no transparent color.")))

(defmethod initialize-instance :after ((frame frame) &rest initargs
                                       &key image &allow-other-keys)
  (declare (ignorable initargs))
  (when image
    (unless (slot-boundp frame 'height)
      (setf (height frame) (height image)))
    (unless (slot-boundp frame 'width)
      (setf (width frame) (width image))))
  (unless (slot-boundp frame 'data)
    (setf (data frame)
          (make-array (* (height frame) (width frame))
                      :element-type '(unsigned-byte 8)
                      :initial-element 0))))
  

(defmethod initialize-instance :after ((frame frame)
                                       &key image height width
                                       &allow-other-keys)
  (when image
    (when (not (or height width))
      (setf (height frame) (height image)
            (width frame) (width image)))
    (vector-push-extend frame (frames image))))

(defmethod (setf image) :after (image (frame frame))
  (unless (slot-boundp frame 'height)
    (setf (height frame) (height image)))
  (unless (slot-boundp frame 'width)
    (setf (width frame) (width image)))
  (vector-push-extend frame (frames image)))


(defvar *disposal-methods*
  '((:unspecified . 0)
    (:none . 1)
    (:restore-background . 2)
    (:restore-previous . 3)))

(defun disposal-method-value (keyword)
  (or (cdr (assoc keyword *disposal-methods*)) 0))


(defun write-block-terminator (stream)
  (write-byte 0 stream))

(defun boolean-bit (value)
  (if value 1 0))


;;; Spec from http://members.aol.com/royalef/gifabout.htm
(defvar *netscape-signature*
  (make-array 11
              :element-type '(unsigned-byte 8)
              :initial-contents '(78 69 84 83 67 65 80 69 50 46 48))
  "The letters of `NETSCAPE2.0' as ASCII octets.")

(defun write-netscape-looping-block (stream)
  (write-byte #x21 stream)
  (write-byte #xFF stream)
  (write-byte (length *netscape-signature*) stream)
  (write-sequence *netscape-signature* stream)
  (write-byte 3 stream)
  (write-byte 1 stream)
  (write-uint16 #xFFFF stream)
  (write-byte 0 stream))

(defun write-comment (comment stream)
  "Write COMMENT to the GIF. Since the characters must be ASCII,
replace any out-of-range character codes with #\\Space."
  (flet ((cleaned-char-code (char)
           (let ((code (char-code char)))
             (if (> code 127) 32 code))))
    (write-byte #x21 stream)
    (write-byte #xFE stream)
    (write-byte (length comment) stream)
    (loop for char across comment do
          (write-byte (cleaned-char-code char) stream))
    (write-block-terminator stream)))

(defun write-graphic-control-block (frame stream)
  ;; extension introducer
  (write-byte #x21 stream)
  ;; graphic control label
  (write-byte #xF9 stream)
  ;; block size
  (write-byte 4 stream)
  ;; packed filds: RRRDDDUT
  ;; RRR = reserved, DDD = disposal method, U = user input, T =
  ;; transparent color flag
  (write-byte (logior (ash (disposal-method-value (disposal-method frame)) 2)
                      (boolean-bit (transparency-index frame)))
              stream)
  ;; delay time
  (write-uint16 (delay-time frame) stream)
  ;; transparent color index
  (write-byte (or (transparency-index frame) 0) stream)
  ;; block terminator
  (write-block-terminator stream))

(defun color-table-size (color-table)
  (if color-table
      (integer-length (1- (/ (length color-table) 3)))
      1))

(defun write-color-table (color-table stream)
  (write-sequence color-table stream))
       
(defmethod bpp ((frame frame))
  (let ((color-table (or (color-table frame)
                         (color-table (image frame))
                         (error "No color table available!"))))
    (color-table-size color-table)))

(defmethod bpp ((image image))
  2)


(defun write-frame (frame stream)
  (write-graphic-control-block frame stream)
  (write-byte +image-separator-code+ stream)
  (write-uint16 (offset-left frame) stream)
  (write-uint16 (offset-top frame) stream)
  (write-uint16 (width frame) stream)
  (write-uint16 (height frame) stream)
  ;; packed byte: CISRRSSS
  ;; C = local color table flag, I = interlaced flag, RR = reserved,
  ;; SSS = size of color table, minus one (aka bit depth, less one)
  (write-byte (logior (ash (boolean-bit (color-table frame)) 7)
                      (ash (boolean-bit (interlacedp frame)) 6)
                      (1- (color-table-size (color-table frame))))
              stream)
  (when (color-table frame)
    (write-color-table (color-table frame) stream))
  (write-byte (bpp frame) stream)
  (lzw-compress (data frame) (bpp frame) stream)
  (write-block-terminator stream))
  
(defun write-image-header (image stream)
  (write-sequence *gif-signature* stream)
  (write-uint16 (width image) stream)
  (write-uint16 (height image) stream)
  ;; packed byte: GRRRSTTT
  ;; G = global color table flag, RRR = color resolution, S = sort flag,
  ;; TTT = global color table size
  (write-byte (logior (ash (boolean-bit (color-table image)) 7)
                      (1- (color-table-size (color-table image))))
              stream)
  ;; background color index
  (write-byte 0 stream)
  (write-byte +pixel-aspect-ratio+ stream)
  (when (color-table image)
    (write-color-table (color-table image) stream))
  (when (comment image)
    (write-comment (comment image) stream))
  (when (loopingp image)
    (write-netscape-looping-block stream)))

(defun write-image (image stream)
  (write-image-header image stream)
  (loop for frame across (frames image) do
        (write-frame frame stream))
  (write-byte +gif-trailer-code+ stream))

(defun last-frame (image)
  (let* ((frames (frames image))
         (i (fill-pointer frames)))
    (unless (zerop i)
      (aref frames (1- i)))))

(defun make-frame (image
                   &rest initargs
                   &key height width offset-top offset-left color-table)
  (let ((frame (apply #'make-instance 'frame :image image initargs)))
    (vector-push-extend frame (frames image))
    frame))



;;; Making scratch data

(defun random-color-table (size)
  (let ((data (make-array (* size 3) :element-type '(unsigned-byte 8))))
    (dotimes (i (* size 3) data)
      (setf (aref data i) (random 256)))))

(defun random-image-data (height width max)
  (let ((data (make-array (* height width) :element-type '(unsigned-byte 8))))
    (dotimes (i (* height width) data)
      (setf (aref data i) (random max)))))

(defun random-frame (image)
  (vector-push-extend
   (make-instance 'frame
                  :height (height image)
                  :width (width image)
                  :image image
                  :delay-time 10
                  :data (random-image-data (height image) (width image)
                                           (/ (length (color-table image))
                                              3)))
   (frames image)))
        
  

(defun output-image (image file)
  (with-open-file (stream file
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-image image stream)
    (probe-file file)))
