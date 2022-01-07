;;; Tests of mh:create-folder.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest create-folder ('(t t "folder") create-folder-1)
  "Test `create-folder' with an empty set of folders."
  (with-test-dir (dir)
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: ~A~%" dir)))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (mh:create-folder "folder")
       (list (if (probe-file (merge-pathnames "folder/" dir)) t)
	     (eq (file-mode (merge-pathnames "folder/" dir)) #o755)
	     (with-open-file (stream (merge-pathnames "folders" dir))
	       (lisp:read-line stream ())))))))

(deftest create-folder (t create-folder-2)
  "Test `create-folder', creating many folders."
  (with-test-dir (dir "Mail/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: Mail~%" dir)))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (mh:create-folder "one")
       (mh:create-folder "two")
       (mh:create-folder "three")
       (and (probe-file "home:Mail/one/")
	    (eq (file-mode "home:Mail/one/") #o755)
	    (probe-file "home:Mail/two/")
	    (eq (file-mode "home:Mail/two/") #o755)
	    (probe-file "home:Mail/three/")
	    (eq (file-mode "home:Mail/three/") #o755)
	    (string= "one
three
two
"
		     (with-output-to-string (out)
		       (with-open-file (in "home:Mail/folders")
			 (transfer in out)))))))))

(deftest create-folder (t create-folder-3)
  "Test `create-folder', amongst existing folders."
  (with-test-dir (dir "Mail/one/" "Mail/two/" )
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%"))
      (letf (((search-list "home:") dir))
	(mh:with-fresh-state
	 (mh:create-folder "three")
	 (and (probe-file "home:Mail/one/")
	      (eq (file-mode "home:Mail/one/") #o755)
	      (probe-file "home:Mail/two/")
	      (eq (file-mode "home:Mail/two/") #o755)
	      (probe-file "home:Mail/three/")
	      (eq (file-mode "home:Mail/three/") #o755)
	      (string= "one
three
two
"
		       (with-output-to-string (out)
			 (with-open-file (in "home:Mail/folders")
			   (transfer in out))))))))))

(deftest create-folder (t create-folder-4)
  "Test `create-folder', creating a subfolder in an empty set of folders."
  (with-test-dir (dir "Mail/")
   (letf (((search-list "home:") dir))
     (in-directory dir
       (with-open-file (stream ".mh_profile"
			       :direction :output
			       :if-exists :error
			       :if-does-not-exist :create)
	 (format stream "Path: Mail~%" dir)))
     (mh:with-fresh-state
      (mh:create-folder "folder/sub")
      (and (probe-file "home:Mail/folder/sub")
	   (eq (file-mode "home:Mail/folder/sub") #o755)
	  (string= "folder/sub
"
		   (with-output-to-string (out)
		     (with-open-file (in "home:Mail/folders")
		       (transfer in out)))))))))

(deftest create-folder (t create-folder-5)
  "Test `create-folder', creating many subfolders."
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
       (mh:create-folder "another/subfolder/three")
       (and (probe-file "home:Mail/folder/one/")
	    (eq (file-mode "home:Mail/folder/one/") #o755)
	    (probe-file "home:Mail/folder/two/")
	    (eq (file-mode "home:Mail/folder/two") #o755)
	    (probe-file "home:Mail/another/subfolder/three/")
	    (eq (file-mode "home:Mail/another/subfolder/three") #o755)
	    (string= "another/subfolder/three
folder/one
folder/two
"
		     (with-output-to-string (out)
		       (with-open-file (in "home:Mail/folders")
			 (transfer in out)))))))))

(deftest create-folder (t create-folder-6)
  "Test `create-folder', creating a subfolder amongst existing folders."
  (with-test-dir (dir "Mail/one/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (mh:create-folder "two/three")
       (and (probe-file "home:Mail/one/")
	    (eq (file-mode "home:Mail/one/") #o755)
	    (probe-file "home:Mail/two/three/")
	    (eq (file-mode "home:Mail/two/three/") #o755)
	    (string= "one
two/three
"
		     (with-output-to-string (out)
		       (with-open-file (in "home:Mail/folders")
			 (transfer in out)))))))))

(deftest create-folder (t create-folder-7)
  "Test `create-folder' with a leading + on the folder name."
  (with-test-dir (dir "Mail/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (mh:create-folder "+folder")
       (and (probe-file "home:Mail/folder/")
	    (eq (file-mode "home:Mail/folder/") #o755)
	    (string= "folder"
		     (with-open-file (stream "home:Mail/folders")
		       (lisp:read-line stream ()))))))))

(deftest create-folder (t create-folder-8)
  "Test `create-folder' with a leading + and a trailing slash on the folder
   name."
  (with-test-dir (dir "Mail/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (mh:create-folder "+folder/")
       (and (probe-file "home:Mail/folder/")
	    (eq (file-mode "home:Mail/folder/") #o755)
	    (string= "folder"
		     (with-open-file (stream "home:Mail/folders")
		       (lisp:read-line stream ()))))))))


;; Component "folder-protect".

(deftest create-folder (t create-folder-20)
  "Test `create-folder' with a folder-protect component."
  (with-test-dir (dir "Mail/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Folder-Protect: 711~%")
	(format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (mh:create-folder "+folder")
       (and (probe-file "home:Mail/folder/")
	    (eq (file-mode "home:Mail/folder/") #o711)
	    (string= "folder"
		     (with-open-file (stream "home:Mail/folders")
		       (lisp:read-line stream ()))))))))


;;;; Errors.

(deftest create-folder (t create-folder-40)
  "Test `create-folder' with an empty argument."
  (let (ret)
    (handler-case
	(mh:create-folder ())
      (error () (setq ret t)))
    ret))

(deftest create-folder (() create-folder-41)
  "Test `create-folder', creating an already existing folder."
  (with-test-dir (dir "Mail/folder/")
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
	     (mh:create-folder "+folder")
	   (error () (setq ret t)))
	 ret)))))

(deftest create-folder (t create-folder-42)
  "Test `create-folder' with an erroneous folder-protect component."
  (with-test-dir (dir "Mail/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Folder-Protect: error~%")
	(format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (let ((ret))
	 (handler-case
	     (mh:create-folder "folder")
	   (error () (setq ret t)))
	 ret)))))
