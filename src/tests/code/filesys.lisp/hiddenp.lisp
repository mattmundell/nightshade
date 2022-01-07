;;; Tests of lisp:hiddenp.

(in-package "LISP")

(import '(deftest:deftest))


;;; Files and directories.

(deftest hiddenp (() hiddenp-5)
  "Test `hiddenp' with a visible directory."
  (let ((dir (pick-new-dir "/tmp/tmp-~D-~D")))
    (unwind-protect
	(and (hiddenp (namify dir))
	     (hiddenp (directorify dir)))
      (delete-dir dir))))

(deftest hiddenp (t hiddenp-6)
  "Test `hiddenp' with a hidden directory."
  (let ((dir (pick-new-dir "/tmp/.tmp-~D-~D/")))
    (unwind-protect
	(and (hiddenp (namify dir))
	     (hiddenp (directorify dir)))
      (delete-dir dir))))

(deftest hiddenp (() hiddenp-7)
  "Test `hiddenp' with a visible directory with an extension."
  (let ((dir (pick-new-dir "/tmp/tmp-~D-~D.ext/")))
    (unwind-protect
	(and (hiddenp (namify dir))
	     (hiddenp (directorify dir)))
      (delete-dir dir))))

(deftest hiddenp (t hiddenp-8)
  "Test `hiddenp' with a hidden directory with an extension."
  (let ((dir (pick-new-dir "/tmp/.tmp-~D-~D.ext/")))
    (unwind-protect
	(and (hiddenp (namify dir))
	     (hiddenp (directorify dir)))
      (delete-dir dir))))

(deftest hiddenp (() hiddenp-9)
  "Test `hiddenp' with a visible file."
  (let ((file (pick-new-file "/tmp/tmp-~D-~D")))
    (unwind-protect
	(hiddenp file)
      (delete-file file))))

(deftest hiddenp (() hiddenp-10)
  "Test `hiddenp' with a visible file with an extension."
  (let ((file (pick-new-file "/tmp/tmp-~D-~D.ext")))
    (unwind-protect
	(hiddenp file)
      (delete-file file))))

(deftest hiddenp (t hiddenp-11)
  "Test `hiddenp' with a hidden file."
  (let ((file (pick-new-file "/tmp/.tmp-~D-~D")))
    (unwind-protect
	(hiddenp file)
      (delete-file file))))

(deftest hiddenp (t hiddenp-12)
  "Test `hiddenp' with a hidden file with an extension."
  (let ((file (pick-new-file "/tmp/.tmp-~D-~D.ext")))
    (unwind-protect
	(hiddenp file)
      (delete-file file))))


;;; Symlinks to files.

(deftest hiddenp (() hiddenp-13)
  "Test `hiddenp' on a visible file via a visible symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (hiddenp link)
	    (delete-file link)))
      (delete-file file))))

(deftest hiddenp (t hiddenp-14)
  "Test `hiddenp' on a hidden file via a visible symlink."
  (let ((file (pick-new-file "/tmp/.file-~D-~D"))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (hiddenp link)
	    (delete-file link)))
      (delete-file file))))

(deftest hiddenp (() hiddenp-15)
  "Test `hiddenp' on a visible symlink to a hidden file."
  (let ((file (pick-new-file "/tmp/.file-~D-~D"))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (hiddenp link :check-for-links t)
	    (delete-file link)))
      (delete-file file))))

(deftest hiddenp (() hiddenp-16)
  "Test `hiddenp' on a visible file via a hidden symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file "/tmp/.link-~D-~D")))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (hiddenp link)
	    (delete-file link)))
      (delete-file file))))

(deftest hiddenp (t hiddenp-17)
  "Test `hiddenp' on a hidden symlink to a visible file."
  (let ((file (pick-new-file))
	(link (pick-new-file "/tmp/.link-~D-~D")))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (hiddenp link :check-for-links t)
	    (delete-file link)))
      (delete-file file))))

(deftest hiddenp (t hiddenp-18)
  "Test `hiddenp' on a hidden file via a hidden symlink."
  (let ((file (pick-new-file "/tmp/.file-~D-~D"))
	(link (pick-new-file "/tmp/.link-~D-~D")))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (hiddenp link)
	    (delete-file link)))
      (delete-file file))))


;;; Symlinks to directories.

(deftest hiddenp (() hiddenp-19)
  "Test `hiddenp' on a visible directory via a visible symlink."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (hiddenp (namify link))
		   (hiddenp (directorify link)))
	    (delete-file link)))
      (delete-dir dir))))

(deftest hiddenp (() hiddenp-20)
  "Test `hiddenp' on a visible directory via a hidden symlink."
  (let ((dir (pick-new-dir))
	(link (pick-new-file "/tmp/.tmp-~D-~D")))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (hiddenp (namify link))
		   (hiddenp (directorify link)))
	    (delete-file link)))
      (delete-dir dir))))

(deftest hiddenp (t hiddenp-21)
  "Test `hiddenp' on a hidden directory via a visible symlink."
  (let ((dir (pick-new-dir "/tmp/.tmp-~D-~D"))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (hiddenp (namify link))
		   (hiddenp (directorify link)))
	    (delete-file link)))
      (delete-dir dir))))

(deftest hiddenp (t hiddenp-22)
  "Test `hiddenp' on a hidden directory via a hidden symlink."
  (let ((dir (pick-new-dir "/tmp/.tmp-~D-~D/"))
	(link (pick-new-file "/tmp/.tmp-~D-~D")))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (hiddenp (namify link))
		   (hiddenp (directorify link)))
	    (delete-file link)))
      (delete-dir dir))))

(deftest hiddenp (() hiddenp-23)
  "Test `hiddenp' on a visible symlink to a visible directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (hiddenp (namify link) :check-for-links t)
		   (hiddenp (directorify link)
			    :check-for-links t))
	    (delete-file link)))
      (delete-dir dir))))

(deftest hiddenp (t hiddenp-24)
  "Test `hiddenp' on a hidden symlink to a visible directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file "/tmp/.tmp-~D-~D")))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (hiddenp (namify link) :check-for-links t)
		   (hiddenp (directorify link) :check-for-links t))
	    (delete-file link)))
      (delete-dir dir))))

(deftest hiddenp (() hiddenp-25)
  "Test `hiddenp' on a visible symlink to a hidden directory."
  (let ((dir (pick-new-dir "/tmp/.tmp-~D-~D"))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (hiddenp (namify link) :check-for-links t)
		   (hiddenp (directorify link) :check-for-links t))
	    (delete-file link)))
      (delete-dir dir))))

(deftest hiddenp (t hiddenp-26)
  "Test `hiddenp' on a hidden symlink to a hidden directory."
  (let ((dir (pick-new-dir "/tmp/.tmp-~D-~D/"))
	(link (pick-new-file "/tmp/.tmp-~D-~D")))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (hiddenp (namify link) :check-for-links t)
		   (hiddenp (directorify link) :check-for-links t))
	    (delete-file link)))
      (delete-dir dir))))


;;; Broken symlinks.

(deftest hiddenp (t hiddenp-30)
  "Test `hiddenp' with a hidden broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file "/tmp/.link-~D-~D")))
    (delete-file link)
    (symlink-file link file)
    (delete-file file)
    (unwind-protect
	(hiddenp link :check-for-links t)
      (delete-file link))))

(deftest hiddenp (() hiddenp-31)
  "Test `hiddenp' with a broken visible symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (symlink-file link file)
    (delete-file file)
    (unwind-protect
	(hiddenp link :check-for-links t)
      (delete-file link))))

(deftest hiddenp (t hiddenp-32)
  "Test `hiddenp' with the destination of a hidden broken."
  (let ((file (pick-new-file))
	(link (pick-new-file "/tmp/.link-~D-~D"))
	ret)
    (delete-file link)
    (symlink-file link file)
    (delete-file file)
    (unwind-protect
	(handler-case
	    (hiddenp link)
	  (error () (setq ret t)))
      (delete-file link))
    ret))


;;; Current directory, and "up".

(deftest hiddenp (t hiddenp-40)
  "Test `hiddenp' with the name for the current directory."
  (hiddenp "."))

(deftest hiddenp (t hiddenp-41)
  "Test `hiddenp' with the name for the parent directory."
  (hiddenp ".."))
