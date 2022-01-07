;;; -*- Log: code.log; Package: FTP -*-
;;;
;;; File Transfer Protocol client functions.

(in-package "FTP")

(export '(ftp-read ftp-write))

(defun ftp-read (pathname stream)
  "Read file at FTP Pathname into Stream."
  (declare (ignore pathname))
  (write-string "read test" stream))

(defun ftp-write (pathname stream)
  "Write from stream into file at FTP Pathname."
  (declare (ignore pathname))
  (error "ftp-write"))
