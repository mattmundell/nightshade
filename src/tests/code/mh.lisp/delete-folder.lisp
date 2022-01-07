;;; Tests of mh:delete-folder.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest delete-folder ('(() 0) delete-folder-1)
  "Test `delete-folder' on a single folder."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
      (letf (((search-list "home:") dir))
	(mh:with-fresh-state
	 (mh:create-folder "folder")
	 (mh:delete-folder "folder")
	 (list (probe-file "home:Mail/folder/")
	       (length
		(with-output-to-string (out)
		  (with-open-file (in "home:Mail/folders")
		    (transfer in out))))))))))

(deftest delete-folder (() delete-folder-2)
  "Test `delete-folder' with a leading + on the folder name."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
	(mh:create-folder "folder")
	(mh:delete-folder "+folder")
	(or (probe-file "home:Mail/folder/")
	    (plusp
	     (length
	      (with-output-to-string (out)
		(with-open-file (in "home:Mail/folders")
		  (transfer in out)))))))))))

(deftest delete-folder (t delete-folder-3)
  "Test `delete-folder', deleting one of many folders."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
	(mh:create-folder "one")
	(mh:create-folder "two")
	(mh:create-folder "three")
	(mh:delete-folder "three")
	(and (probe-file "home:Mail/one/")
	     (eq (file-mode "home:Mail/one/") #o755)
	     (probe-file "home:Mail/two/")
	     (eq (file-mode "home:Mail/two/") #o755)
	     (fi (probe-file "home:Mail/three/")
		 (string= "one
two
"
			  (with-output-to-string (out)
			    (with-open-file (in "home:Mail/folders")
			      (transfer in out)))))))))))

(deftest delete-folder (() delete-folder-4)
  "Test `delete-folder' on a single subfolder."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
	(mh:create-folder "folder")
	(mh:create-folder "folder/subfolder")
	(mh:delete-folder "folder/subfolder")
	(if (and (probe-file "home:Mail/folder")
		 (string= "folder
"
			  (with-output-to-string (out)
			    (with-open-file (in "home:Mail/folders")
			      (transfer in out)))))
	    (probe-file "home:Mail/folder/subfolder")
	    t))))))

(deftest delete-folder (() delete-folder-5)
  "Test `delete-folder' with a leading + on a subfolder name."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
	(mh:create-folder "folder/subfolder")
	(mh:delete-folder "+folder/subfolder")
	(if (zerop (length (with-output-to-string (out)
			     (with-open-file (in "home:Mail/folders")
			       (transfer in out)))))
	    (probe-file "home:Mail/folder/subfolder")
	    t))))))

(deftest delete-folder (t delete-folder-6)
  "Test `delete-folder', deleting one of many subfolders."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
	(mh:create-folder "folder/one")
	(mh:create-folder "folder/two")
	(mh:create-folder "folder/three")
	(mh:delete-folder "folder/three")
	(and (probe-file "home:Mail/folder/one/")
	     (eq (file-mode "home:Mail/folder/one/") #o755)
	     (probe-file "home:Mail/folder/two/")
	     (eq (file-mode "home:Mail/folder/two/") #o755)
	     (fi (probe-file "home:Mail/folder/three/")
		 (string= "folder/one
folder/two
"
			  (with-output-to-string (out)
			    (with-open-file (in "home:Mail/folders")
			      (transfer in out)))))))))))

(deftest delete-folder (t delete-folder-7)
  "Test `delete-folder', deleting one of many subfolders with a trailing
   slash on the folder name."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
	(mh:create-folder "folder/one")
	(mh:create-folder "folder/two")
	(mh:create-folder "folder/three")
	(mh:delete-folder "folder/three/")
	(and (probe-file "home:Mail/folder/one/")
	     (eq (file-mode "home:Mail/folder/one/") #o755)
	     (probe-file "home:Mail/folder/two/")
	     (eq (file-mode "home:Mail/folder/two/") #o755)
	     (fi (probe-file "home:Mail/folder/three/")
		 (string= "folder/one
folder/two
"
			  (with-output-to-string (out)
			    (with-open-file (in "home:Mail/folders")
			      (transfer in out)))))))))))

(deftest delete-folder (t delete-folder-8)
  "Test `delete-folder', where the directory is kept due to extra files."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
	(mh:create-folder "folder")
	(in-directory (mh:folder-pathname "folder")
	  (touch-file "1.bak")
	  (touch-file "1")
	  (touch-file "10")
	  (touch-file "139")
	  (touch-file "file")
	  (touch-file "1.BAK"))
	(mh:delete-folder "folder")
	(and (probe-file "home:Mail/folder")
	     (zerop (length (with-output-to-string (out)
			      (with-open-file (in "home:Mail/folders")
				(transfer in out)))))))))))

(deftest delete-folder (() delete-folder-9)
  "Test `delete-folder', where the directory has files."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
	(mh:create-folder "folder")
	(in-directory (mh:folder-pathname "folder")
	  (touch-file "1")
	  (touch-file "10")
	  (touch-file "139")
	  (touch-file ".file")
	  (touch-file ",file")
	  (touch-file "+file")
	  (touch-file "_file")
	  (touch-file "#file")
	  (touch-file "@")
	  (touch-file ",")
	  (touch-file "cur"))
	(mh:delete-folder "folder")
	(or (probe-file "home:Mail/folder")
	    (plusp (length (with-output-to-string (out)
			     (with-open-file (in "home:Mail/folders")
			       (transfer in out)))))))))))


;;;; Errors.

(deftest delete-folder (t delete-folder-40)
  "Test `delete-folder' with an empty argument."
  (let (ret)
    (handler-case
	(mh:delete-folder ())
      (error () (setq ret t)))
    ret))

(deftest delete-folder (t delete-folder-41)
  "Test `delete-folder' with a missing folder."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
      (letf (((search-list "home:") dir))
	(mh:with-fresh-state
	 (let ((ret))
	   (handler-case
	       (mh:delete-folder "folder")
	     (error () (setq ret t)))
	   ret))))))

(deftest delete-folder (() delete-folder-42)
  "Test `delete-folder' on a missing subfolder."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
      (letf (((search-list "home:") dir))
	(mh:with-fresh-state
	 (let ((ret))
	   (handler-case
	       (mh:delete-folder "folder/sub")
	     (error () (setq ret ())))
	   ret))))))

(deftest delete-folder (t delete-folder-43)
  "Test `delete-folder' where the folder directory has been write-blocked."
  (with-test-dir (config-dir)
    (with-test-dir (dir "Mail/")
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
      (letf (((search-list "home:") dir))
	(mh:with-fresh-state
	 (mh:create-folder "folder/one")
	 (mh:create-folder "folder/two")
	 (mh:create-folder "folder/three")
	 (setf (file-mode (mh:folder-pathname "folder/three")) "a-rwx")
	 (let ((ret))
	   (handler-case
	       (mh:delete-folder "folder/three/")
	     (error () (setq ret t)))
	   (setf (file-mode (mh:folder-pathname "folder/three")) "a+rwx")
	   ret))))))
