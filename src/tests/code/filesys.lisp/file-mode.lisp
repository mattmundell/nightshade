;;; Tests of lisp:file-mode.

(in-package "LISP")

(import '(deftest:deftest))

(deftest file-mode ((logior unix:execall
			    unix:readall
			    unix:writeall)
		    file-mode-0)
  "Test `file-mode' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(file-mode dir)
      (delete-dir dir))))

(deftest file-mode (#o666 file-mode-1)
  "Test `file-mode' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(file-mode file)
      (delete-file file))))

(deftest file-mode (#o666 file-mode-2)
  "Test `file-mode' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (file-mode link))
      (delete-file file)
      (delete-file link))))

#|
(deftest file-mode (:link file-mode-3)
  "Test `file-mode' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (file-mode link :check-for-links t))
      (delete-file file)
      (delete-file link))))
|#

(deftest file-mode ((logior unix:execall
			    unix:readall
			    unix:writeall)
		    file-mode-4)
  "Test `file-mode' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (file-mode link))
      (delete-file link)
      (delete-dir dir))))

#|
(deftest file-mode (:link file-mode-5)
  "Test `file-mode' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (file-mode link :check-for-links t))
      (delete-file link)
      (delete-dir dir))))

(deftest file-mode (:link file-mode-6)
  "Test `file-mode' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (file-mode link :check-for-links t))
      (delete-file link))))

(deftest file-mode (() file-mode-7)
  "Test `file-mode' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (file-mode link))
      (delete-file link))))
|#

(deftest file-mode ((logior unix:readall
			    unix:writeall)
		    file-mode-8)
  "Test `file-mode' with a special file."
  (file-mode "/dev/null"))


;;;; setf

(deftest file-mode ((logior unix:execgrp
			    unix:readall
			    unix:writeall)
		    file-mode-10)
  "Test `file-mode' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(setf (file-mode dir) "ou-x")
      (delete-dir dir))))

(deftest file-mode ((logior unix:execgrp
			    unix:readall
			    unix:writeall)
		    file-mode-11)
  "Test `file-mode' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(setf (file-mode file) "g+x")
      (delete-file file))))

(deftest file-mode (#o466 file-mode-12)
  "Test `file-mode' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (setf (file-mode link) #o466))
      (delete-file file)
      (delete-file link))))

#|
(deftest file-mode (:link file-mode-13)
  "Test `file-mode' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (file-mode link :check-for-links t))
      (delete-file file)
      (delete-file link))))
|#

(deftest file-mode (unix:execown file-mode-14)
  "Test `file-mode' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (setf (file-mode link) unix:execown))
      (delete-file link)
      (delete-dir dir))))

#|
(deftest file-mode (:link file-mode-15)
  "Test `file-mode' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (file-mode link :check-for-links t))
      (delete-file link)
      (delete-dir dir))))
|#

#| FIX namespace qualifier fixed, still need check-for-links
(deftest file-mode (:link file-mode-16)
  "Test `file-mode' with a broken symlink.  This showed a missing unix
   namespace qualifier in `set-file-mode'."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (setf (file-mode link :check-for-links t) "a+x"))
      (delete-file link))))
|#

#|
(deftest file-mode (() file-mode-17)
  "Test `file-mode' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (file-mode link))
      (delete-file link))))
|#

;; FIX maybe make a special file to test setting the mode
